% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(mnesia_db).
-include_lib("stdlib/include/qlc.hrl").

% Initialization, startup
-export([init/0]).

% REST functions, to be called by the REST server (from outside via cowboy)
-export([rest_list/1, rest_lookup/2, rest_create/3, rest_delete/2]).

% work functions, to be called by the eIM code (from inside)
-export([work_fetch/2, work_pickup/1, work_pickup/2, work_update/2, work_bind/2, work_finish/3]).

% euicc functions, to be called by the eIM code (from inside)
-export([euicc_counter_tick/1, euicc_counter_get/1]).

% debugging
-export([dump_rest/0, dump_work/0, dump_euicc/0]).

% trigger recurring events (called automatically by timer from this module)
-export([cleanup/0, euicc_setparam/0]).

-record(rest, {resourceId :: binary(), facility :: atom(), eidValue :: binary(), order, status :: atom(), timestamp :: integer(), outcome, debuginfo :: binary()}).
-record(work, {pid :: pid(), resourceId :: binary(), transactionId :: binary(), eidValue :: binary(), order, state}).
-record(euicc, {eidValue :: binary(), counterValue :: integer(), consumerEuicc :: boolean()}).

%TODO: We need some mechanism that looks through the work table from time to time and checks the timestemp (field not
%yet created in work) to find stuck work items. We also might also need a similar mechanism for the rest table that
%cleans up stale items.

% Caution: The status (atom) must be either "new", "work", or "done"

% helper function (to be called from a transaction) to set the status of an item in the rest table.
trans_rest_set_status(ResourceId, Status, Outcome, Debuginfo) ->
    Q = qlc:q([X || X <- mnesia:table(rest), X#rest.resourceId == ResourceId]),
    Rows = qlc:e(Q),
    Timestamp = os:system_time(seconds),
    case Rows of
	[Row | []] ->
	    mnesia:write(Row#rest{status=Status, timestamp=Timestamp, outcome=Outcome, debuginfo=Debuginfo}),
	    ok;
	[] ->
	    error;
	_ ->
	    error
    end.

% Initialize databse scheme, tables and check the database for unfinished orders
init() ->

    % Create an initial mnesia scheme.
    mnesia:stop(),
    case mnesia:create_schema([node()]) of
	{error, {_, {already_exists, _}}} ->
	    ok;
	ok ->
	    logger:notice("    mnesia database schema created")
    end,
    ok = mnesia:start(),
    logger:notice("    mnesia started"),

    % The rest table is a persistent table, so even after a crash it will be possible to continue pending orders.
    case mnesia:create_table(rest,
			     [{attributes, record_info(fields, rest)},
			      {disc_copies, [node()]},
			      {type, set}
			     ]) of
	{aborted, {already_exists, rest}} ->
	    ok;
	{atomic, ok} ->
	    logger:notice("    rest table created")
    end,

    % The work table is volatile. It only contains intermediate results. When the eIM is restarted and there is a
    % pending order that is worked on. Then all the work is lost. This is intentional since we want to avoid keeping
    % states that have gotten inconsistent anyway.
    case mnesia:create_table(work,
			     [{attributes, record_info(fields, work)},
			      {type, set}
			     ]) of
	{aborted, {already_exists, work}} ->
	    ok;
	{atomic, ok} ->
	    logger:notice("    work table created")
    end,

    % The euicc table will store the eUICC master data, such as the eID and the counterValue that is required for
    % the replay protection.
    case mnesia:create_table(euicc,
			     [{attributes, record_info(fields, euicc)},
			      {disc_copies, [node()]},
			      {type, set}
			     ]) of
	{aborted, {already_exists, euicc}} ->
	    ok;
	{atomic, ok} ->
	    logger:notice("    euicc table created")
    end,

    % Wait until the mnesia tables become available.
    case mnesia:wait_for_tables([rest, work], 60000) of
	{timeout, _} ->
	    logger:error("    unable to synchronize mnesia tables"),
	    throw("normal operation not possible");
	ok ->
	    ok
    end,

    % Look through the rest table and mark all items that were in state "work" as "done" and put an appropriate outcome
    % into the rest table. This way we tell the REST API user that the processing of the order was interrupted due to a
    % crash/restart of the eIM. The API user is then expected to delete the item and (if necessary) re-submit the order.
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.status == work]),
		    ResourceIds = qlc:e(Q),
		    lists:foreach(fun(ResourceId) -> trans_rest_set_status(ResourceId, done, [{[{procedureError, abortedOrder}]}], none) end, ResourceIds)
	    end,
    {atomic, ok} = mnesia:transaction(Trans),

    % Start recurring event cycles
    ok = cleanup(),
    ok = euicc_setparam(),

    ok.

% Create REST resource (order)
rest_create(Facility, EidValue, Order) ->
    ok = euicc_create_if_not_exist(EidValue),
    ResourceId = uuid:uuid_to_string(uuid:get_v4_urandom()),
    Timestamp = os:system_time(seconds),
    Row = #rest{resourceId=ResourceId, facility=Facility, eidValue=EidValue, order=Order, status=new,
		timestamp=Timestamp, outcome=[], debuginfo=none},
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.resourceId == ResourceId]),
		    Present = qlc:e(Q),
		    case Present of
			[] ->
			    mnesia:write(Row);
			_ ->
			    error
		    end
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    {Result, ResourceId}.

% Lookup REST resource (order)
rest_lookup(ResourceId, Facility) ->
    Trans = fun() ->
		    Q = qlc:q([{X#rest.status, X#rest.timestamp, X#rest.eidValue, X#rest.order, X#rest.outcome, X#rest.debuginfo} ||
				  X <- mnesia:table(rest),
				  X#rest.resourceId == ResourceId, X#rest.facility == Facility]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
	[{Status, Timestamp, EidValue, Order, Outcome, Debuginfo}] ->
	    {Status, Timestamp, EidValue, Order, Outcome, Debuginfo};
	[] ->
	    none;
	_ ->
	    error
    end.

% Delete REST resource (order)
rest_delete(ResourceId, Facility) ->
    OidRest = {rest, ResourceId},
    OidWork = {work, ResourceId},
    Trans = fun() ->
		    QRest = qlc:q([X#rest.resourceId ||
				  X <- mnesia:table(rest),
				  X#rest.resourceId == ResourceId, X#rest.facility == Facility]),
		    RestPresent = qlc:e(QRest),
		    case RestPresent of
			[] ->
			    none;
			_ ->
			    ok = mnesia:delete(OidRest),

			    % There may be an orphaned work item now, which we must also remove. This will also kill
			    % the order in case it is currently in progress.
			    QWork = qlc:q([X#rest.resourceId ||
					      X <- mnesia:table(rest),
					      X#rest.resourceId == ResourceId, X#rest.facility == Facility]),
			    WorkPresent = qlc:e(QWork),
			    case WorkPresent of
				[] ->
				    ok;
				_ ->
				    mnesia:delete(OidWork)
			    end
		    end
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

% List the resource identifiers of currently present REST resources
rest_list(Facility) ->
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.facility == Facility]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

% Start working on an order by creating a an entry in the work table and marking it's status as "work". After calling
% this it is the callers responsibility to handle the work item and call rest_finish_order when the work is done.
work_fetch(EidValue, Pid) ->

    % TODO: Check whether the given PID already works on another item. This would be a forbidden state. One process
    % can only work on one work item at a time. The API user must call work_finish before the next work item
    % can be processed. (maybe it makes sense to finish stale work items with an error status automatically)

    Trans = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(rest),
				    X#rest.eidValue == EidValue, X#rest.status == new, X#rest.facility =/= euicc]),
		    Rows = qlc:e(Q),
		    case Rows of
			[Row | _] ->
			    % Create an entry in the work table
			    WorkRow = #work{pid=Pid, resourceId=Row#rest.resourceId, transactionId=none,
					    eidValue=Row#rest.eidValue, order=Row#rest.order, state=none},
			    case mnesia:write(WorkRow) of
				ok ->
				    % We are now working on this order
				    ok = trans_rest_set_status(Row#rest.resourceId, work, [], none),
				    Row;
				_ ->
				    error
			    end;
			[] ->
			    none;
			_ ->
			    error
		    end
	    end,

    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
	{rest, _, Facility, _, Order, _, _, _, _} ->
	    logger:notice("Work: fetching new work item: EidValue=~p, Pid=~p, Order=~p, Facility=~p", [EidValue, Pid, Order, Facility]),
	    {Facility, Order};
	none ->
	    logger:notice("Work: no work item in database: EidValue=~p, Pid=~p", [EidValue, Pid]),
	    none;
	_ ->
	    logger:error("Work: cannot fetch work item, database error: EidValue=~p, Pid=~p", [EidValue, Pid]),
	    error
	end.

% Bind a work item to a TransactionId. The transactionId has to be a unique identifier that can be used as a secondary
% key to find a work item in the databse. The binding works in two directions. The work item is first searched by its
% pid, when found, the transactionId is updated. In case the pid has become invalid, then the work item is searched
% again by the transactionId and when found, the pid is updated. This function can be called any time after work_fetch
% was called before. It can also be called multiple times.
work_bind(Pid, TransactionId) ->
    % Transaction to update the TransactionId. This is the normal case. A work item starts without having a
    % TransactionId assigned. As soon as a (new) TransactionId becomes known, it is updated using this Transaction.
    TransUpdateTrnsId = fun() ->
				Q = qlc:q([X || X <- mnesia:table(work), X#work.pid == Pid]),
				Rows = qlc:e(Q),
				case Rows of
				    [Row | _] ->
					mnesia:write(Row#work{transactionId=TransactionId});
				    [] ->
					none;
				    _ ->
					error
				end
			end,

    % Transaction to update the PID. This is a corner case that comes into play in case the PID is lost (the
    % process/connection handling this work item has died). We then try to find the work item by the TransactionId
    % and update its PID.
    TransUpdatePid = fun() ->
			     Q = qlc:q([X || X <- mnesia:table(work), X#work.transactionId == TransactionId]),
			     Rows = qlc:e(Q),
			     case Rows of
				 [Row | _] ->
				     mnesia:write(Row#work{pid=Pid});
				 [] ->
				     none;
				 _ ->
				     error
			     end
		     end,

    {atomic, Result} = mnesia:transaction(TransUpdateTrnsId),
    case Result of
        ok ->
	    logger:notice("Work: bound work item to transactionId: Pid=~p, TransactionId=~p", [Pid, TransactionId]),
	    ok;
	none ->
	    {atomic, UpdatePidResult} = mnesia:transaction(TransUpdatePid),
	    case UpdatePidResult of
		ok ->
		    logger:notice("Work: bound work item to PID: Pid=~p, TransactionId=~p", [Pid, TransactionId]),

		    ok;
		none ->
		    logger:error("Work: cannot bind work item, transactionId nor PID found: Pid=~p, TransactionId=~p",
				 [Pid, TransactionId]),
		    error;
		_ ->
		    logger:error("Work: cannot bind work item, database error: Pid=~p, TransactionId=~p, TransUpdatePid",
				 [Pid, TransactionId]),
		    error
	    end;
	_ ->
	    logger:error("Work: cannot bind work item, database error: Pid=~p, TransactionId=~p, TransUpdateTrnsId",
			 [Pid, TransactionId]),
	    error
    end.

% Pickup a work item that is in progress. This function can be called any time after work_fetch was called
% before. It can also be called multiple times.
work_pickup(Pid) ->
    Trans = fun() ->
		    Q = qlc:q([{X#work.eidValue, X#work.order, X#work.state} || X <- mnesia:table(work), X#work.pid == Pid]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
	[{EidValue, Order, State} | _] ->
	    {EidValue, Order, State};
	[] ->
	    logger:error("Work: no work item found under specified Pid, already finished?, not fetched?: Pid=~p", [Pid]),
	    none;
	_ ->
	    logger:error("Work: cannot pick up work item, database error: Pid=~p", [Pid]),
	    error
    end.
work_pickup(Pid, TransactionId) ->
    ok = work_bind(Pid, TransactionId),
    work_pickup(Pid).

% Update a work item that is in progress. This fuction updates the state (any user defined term) of the work item.
% This function can be called any time after work_fetch was called before. It can also be called multiple times.
work_update(Pid, State) ->
    Trans = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(work), X#work.pid == Pid]),
		    Rows = qlc:e(Q),
		    case Rows of
			[Row | _] ->
			    mnesia:write(Row#work{state=State});
			[] ->
			    error;
			_ ->
			    error
		    end
	    end,

    {atomic , Result} = mnesia:transaction(Trans),
    case Result of
        ok ->
	    logger:notice("Work: updating work item: Pid=~p, State=~p", [Pid, State]),
	    ok;
	_ ->
	    logger:error("Work: cannot update, database error: Pid=~p, State=~p", [Pid, State]),
	    error
    end.

% Finish an order that has been worked on. This removes the related entry from the work table and sets the status in
% the rest table to "done".
work_finish(Pid, Outcome, Debuginfo) ->
    Trans = fun() ->
		    Q = qlc:q([X#work.resourceId || X <- mnesia:table(work), X#work.pid == Pid]),
		    Rows = qlc:e(Q),
		    case Rows of
			[] ->
			    error;
			[ResourceId | _] ->
			    Oid = {work, Pid},
			    ok = mnesia:delete(Oid),
			    ok = trans_rest_set_status(ResourceId, done, Outcome, Debuginfo)
		    end
	    end,

    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
        ok ->
	    logger:notice("Work: finishing work item: Pid=~p, Outcome=~p, Debuginfo=~p", [Pid, Outcome, Debuginfo]),
	    ok;
	_ ->
	    logger:error("Work: cannot finish work item, database error: Pid=~p, Outcome=~p, Debuginfo=~p", [Pid, Outcome, Debuginfo]),
	    error
	end.

trans_euicc_create_if_not_exist(EidValue) ->
    {ok, CounterValue} = application:get_env(onomondo_eim, counter_value),
    Row = #euicc{eidValue=EidValue, counterValue=CounterValue, consumerEuicc=false},
    Q = qlc:q([X#euicc.eidValue || X <- mnesia:table(euicc), X#euicc.eidValue == EidValue]),
    Present = qlc:e(Q),
    case Present of
	[] ->
	    mnesia:write(Row);
	_ ->
	    present
    end.

% Create a new eUICC master data entry
euicc_create_if_not_exist(EidValue) ->
    Trans = fun() ->
		    trans_euicc_create_if_not_exist(EidValue)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
        ok ->
	    logger:notice("eUICC: creating new master data entry: eID=~p", [EidValue]),
	    ok;
	present ->
	    ok;
	_ ->
	    logger:error("eUICC: cannot create master data entry, database error: eID=~p", [EidValue]),
	    error
	end.

% get an incremented counterValue (and store the incremented counterValue as the current counterValue)
euicc_counter_tick(EidValue) ->
    Trans = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(euicc), X#euicc.eidValue == EidValue]),
		    Rows = qlc:e(Q),
		    case Rows of
			[Row | _] ->
			    CounterValue = Row#euicc.counterValue + 1,
			    ok = mnesia:write(Row#euicc{counterValue=CounterValue}),
			    {ok, CounterValue};
			[] ->
			    error;
			_ ->
			    error
		    end
	    end,

    {atomic , Result} = mnesia:transaction(Trans),
    case Result of
        {ok, CounterValue} ->
	    logger:notice("eUICC: incrementing counterValue: eID=~p, counter=~p", [EidValue, CounterValue]),
	    {ok, CounterValue};
	_ ->
	    logger:error("eUICC: cannot increment counterValue, database error: eID=~p", [EidValue]),
	    error
    end.

euicc_counter_get(EidValue) ->
    Trans = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(euicc), X#euicc.eidValue == EidValue]),
		    Rows = qlc:e(Q),
		    case Rows of
			[Row | _] ->
			    {ok, Row#euicc.counterValue};
			[] ->
			    error;
			_ ->
			    error
		    end
	    end,

    {atomic , Result} = mnesia:transaction(Trans),
    case Result of
        {ok, CounterValue} ->
	    logger:notice("eUICC: reading current counterValue: eID=~p, counter=~p", [EidValue, CounterValue]),
	    {ok, CounterValue};
	_ ->
	    logger:error("eUICC: cannot read current counterValue, database error: eID=~p", [EidValue]),
	    error
    end.

mark_stuck(Timeout) ->
    % There may be corner cases where the order gets stuck because the other entity suddenly stops responding. In
    % this case the order will stall. When it remains stalled for too long (minutes), we should remove all related
    % items from the work table and put an appropriate outcome (error code) into the rest table.

    TimestampNow = os:system_time(seconds),

    % Remove Resource from work table and set an appropriate status in the rest table
    HandleResource = fun(ResourceId) ->
			     Oid = {work, ResourceId},
			     ok = mnesia:delete(Oid),
			     trans_rest_set_status(ResourceId, done, [{[{procedureError, stuckOrder}]}], none)
		     end,

    % Find all rest resources that stall in status "work" and older than the specified timeout value
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.status == work, TimestampNow - X#rest.timestamp > Timeout]),
		    Rows = qlc:e(Q),
		    case Rows of
			[] ->
			    ok;
			Rows ->
			    [HandleResource(Row) || Row <- Rows],
			    ok
		    end
	    end,

    {atomic, Result} = mnesia:transaction(Trans),
    Result.

mark_noshow(Timeout) ->
    % There may be corner cases where the order is never processed because the related IPAd/eUICC never shows up to
    % fetch the related eUICC package. If we see an order staying in status "new" for too long (days, weeks), we should
    % mark it as "done" and put an appropriate outcome (error code) into the rest table.

    TimestampNow = os:system_time(seconds),

    % Remove Resource from work table and set an appropriate status in the rest table
    HandleResource = fun(ResourceId) ->
			     trans_rest_set_status(ResourceId, done, [{[{procedureError, noshowOrder}]}], none)
		     end,

    % Find all rest resources that stall in status "work" and older than the specified timeout value
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.status == new, TimestampNow - X#rest.timestamp > Timeout]),
		    Rows = qlc:e(Q),
		    case Rows of
			[] ->
			    ok;
			Rows ->
			    [HandleResource(Row) || Row <- Rows],
			    ok
		    end
	    end,

    {atomic, Result} = mnesia:transaction(Trans),
    Result.

delete_expired(Timeout) ->
    % There may be cases where orders stay unmaintained for too long. When an order stays in status "done" for too long
    % (hours, days), than this may mean that the REST API user lost interest. In this case the related items shoud be
    % removed from the rest table after a reasonable timeout.

    TimestampNow = os:system_time(seconds),

    % Remove Resource from the rest table. Since an abandonned order won't have a coresponding work item in the work
    % table we do not have to worry about creating an orphaned work item.
    HandleResource = fun(ResourceId) ->
			     Oid = {rest, ResourceId},
			     ok = mnesia:delete(Oid)
		     end,

    % Find all rest resources that linger in the rest table for a long time and are not in the status "new"
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.status == done, TimestampNow - X#rest.timestamp > Timeout]),
		    Rows = qlc:e(Q),
		    case Rows of
			[] ->
			    ok;
			Rows ->
			    [HandleResource(Row) || Row <- Rows],
			    ok
		    end
	    end,

    {atomic, Result} = mnesia:transaction(Trans),
    Result.

% Run a cleanup cycle the database
cleanup() ->
    {ok, RestTimeoutStuck} = application:get_env(onomondo_eim, rest_timeout_stuck),
    {ok, RestTimeoutNoshow} = application:get_env(onomondo_eim, rest_timeout_noshow),
    {ok, RestTimeoutExpired} = application:get_env(onomondo_eim, rest_timeout_expired),
    RcStalled = mark_stuck(RestTimeoutStuck),
    RcNoshow = mark_noshow(RestTimeoutNoshow),
    RcExpired = delete_expired(RestTimeoutExpired),

    case {RcStalled, RcNoshow, RcExpired} of
        {ok,ok,ok} ->
	    ok;
	_ ->
	    logger:error("Cleanup: database error"),
	    error
    end,

    % Next cleanup in 10 secs.
    {ok, _} = timer:apply_after(10000, mnesia_db, cleanup, []),
    ok.

% Run scheduled eUICC procedures
euicc_setparam() ->
    % An eUICC procedure in the context of this module has nothing to do with any of the procedures specified in
    % GSMA SGP.22 or SGP.32. In this module an eUICC procedure is a virtual procedure were parameters in the
    % euicc table are set.

    %update one specific parameter
    UpdateEuiccParam = fun(EidValue, Name, Value) ->
			       Q = qlc:q([X || X <- mnesia:table(euicc), X#euicc.eidValue == EidValue]),
			       Rows = qlc:e(Q),
			       case Rows of
				   [Row | []] ->
				       case Name of
					   <<"counterValue">> ->
					       mnesia:write(Row#euicc{counterValue=Value});
					   <<"consumerEuicc">> ->
					       mnesia:write(Row#euicc{consumerEuicc=Value});
					   _ ->
					       error
				       end;
				   [] ->
				       error;
				   _ ->
				       error
			       end
		       end,
    HandleParam = fun(ResourceId, EidValue, Param) ->
			  case Param of
			      {[{Name, Value}]} ->
				  case UpdateEuiccParam(EidValue, Name, Value) of
				      ok ->
					  trans_rest_set_status(ResourceId, done, [{[{euiccUpdateResult, ok}]}], none);
				      _ ->
					  trans_rest_set_status(ResourceId, done, [{[{euiccUpdateResult, badParam}]}], none)
				  end;
			      _ ->
				  trans_rest_set_status(ResourceId, done, [{[{euiccUpdateResult, badParamFormat}]}], none)
			  end
		  end,

    % Parse order and process each parameter individually
    HandleResource = fun({ResourceId, EidValue, Order}) ->
			     trans_euicc_create_if_not_exist(EidValue),
			     case Order of
				 {[{<<"euicc">>, ParameterList }]} ->
				     [HandleParam(ResourceId, EidValue, Param) || Param <- ParameterList],
				     ok;
				 _ ->
				     trans_rest_set_status(ResourceId, done, [{[{procedureError, badOrder}]}], none)
			     end
		     end,

    % Look into facility euicc and find the first entry that is in status "new".
    Trans = fun() ->
		    Q = qlc:q([{X#rest.resourceId, X#rest.eidValue, X#rest.order} || X <- mnesia:table(rest), X#rest.status == new, X#rest.facility == euicc]),
		    Rows = qlc:e(Q),
		    case Rows of
			[] ->
			    ok;
			Rows ->
			    [HandleResource(Row) || Row <- Rows],
			    ok
		    end
	    end,

    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
        ok ->
	    ok;
	_ ->
	    logger:error("eUICC: euicc procedure failed, database error"),
	    error
    end,

    % Next euicc procedure in 10 secs.
    {ok, _} = timer:apply_after(10000, mnesia_db, euicc_setparam, []),
    ok.

% Dump all currently pending rest items (for debugging, to be called from console)
dump_rest() ->
    Trans = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(rest)]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

% Dump all currently pending work items (for debugging, to be called from console)
dump_work() ->
    Trans = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(work)]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

% Dump all eUICCs we are aware of
dump_euicc() ->
    Trans = fun() ->
		    Q = qlc:q([X || X <- mnesia:table(euicc)]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.

