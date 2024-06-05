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

-record(rest, {resourceId :: binary(), facility :: atom(), eidValue :: binary(), order, status :: atom(), timestamp :: integer(), outcome, debuginfo :: binary()}).
-record(work, {pid :: pid(), resourceId :: binary(), transactionId :: binary(), eidValue :: binary(), order, state}).
-record(euicc, {eidValue :: binary(), counterValue :: integer()}).

%TODO: We need some mechanism that looks through the work table from time to time and checks the timestemp (field not
%yet created in work) to find stuck work items. We also might also need a similar mechanism for the rest table that
%cleans up stale items.

% helper function (to be called from a transaction) to set the status of an item in the rest table.
trans_rest_set_status(ResourceId, Status, Outcome, Debuginfo) ->
    Q = qlc:q([X || X <- mnesia:table(rest), X#rest.resourceId == ResourceId]),
    Rows = qlc:e(Q),
    Timestamp = uuid:get_v1_time(),
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

    % Look through the rest table and mark all items that were in state "work" as "aborted". This way we tell the REST
    % API user that the processing of the order was interrupted. The API user is then expected to delete the item and
    % re-submit the order.
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.status == work]),
		    ResourceIds = qlc:e(Q),
		    lists:foreach(fun(ResourceId) -> trans_rest_set_status(ResourceId, aborted, none, none) end, ResourceIds)
	    end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

% Create REST resource (order)
rest_create(Facility, EidValue, Order) ->
    ok = euicc_create_if_not_exist(EidValue),
    ResourceId = uuid:uuid_to_string(uuid:get_v4_urandom()),
    Timestamp = uuid:get_v1_time(),
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
    Oid = {rest, ResourceId},
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId ||
				  X <- mnesia:table(rest),
				  X#rest.resourceId == ResourceId, X#rest.facility == Facility]),
		    Present = qlc:e(Q),
		    case Present of
			[] ->
			    none;
			_ ->
			    mnesia:delete(Oid)
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
				    X#rest.eidValue == EidValue, X#rest.status == new]),
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
					mnesia:write(Row#work{transactionId=TransactionId}),
					ok;
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
				     mnesia:write(Row#work{pid=Pid}),
				     ok;
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
			    mnesia:write(Row#work{state=State}),
			    ok;
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
			    Oid = {work, ResourceId},
			    mnesia:delete(Oid),
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

% Create a new eUICC master data entry
euicc_create_if_not_exist(EidValue) ->
    {ok, CounterValue} = application:get_env(onomondo_eim, counter_value),
    Row = #euicc{eidValue=EidValue, counterValue=CounterValue},
    Trans = fun() ->
		    Q = qlc:q([X#euicc.eidValue || X <- mnesia:table(euicc), X#euicc.eidValue == EidValue]),
		    Present = qlc:e(Q),
		    case Present of
			[] ->
			    mnesia:write(Row);
			_ ->
			    present
		    end
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
        ok ->
	    logger:notice("eUICC: creating new master data entry: eID=~p, counter=~p", [EidValue, CounterValue]),
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
			    mnesia:write(Row#euicc{counterValue=CounterValue}),
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

