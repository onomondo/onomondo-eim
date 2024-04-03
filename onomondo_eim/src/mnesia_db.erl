% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(mnesia_db).
-include_lib("stdlib/include/qlc.hrl").

% Initialization, startup
-export([init/0]).

% REST functions, to be called by the REST server (from outside via cowboy)
-export([rest_list/1, rest_lookup/2, rest_create/3, rest_delete/2]).

% work functions, to be called by the eIM code (from inside)
-export([work_fetch/2, work_pickup/1, work_update/2, work_finish/3]).

% debugging
-export([dump_rest/0, dump_work/0]).

-record(rest, {resourceId :: binary(), facility :: atom(), eidValue :: binary(), order, status :: atom(), timestamp :: integer(), outcome:: binary()}).
-record(work, {pid :: pid(), resourceId :: binary(), order, state}).

%TODO: We need some mechanism that looks through the work table from time to time and checks the timestemp (field not
%yet created in work) to find stuck work items. We also might also need a similar mechanism for the rest table that
%cleans up stale items.

% helper function (to be called from a transaction) to set the status of an item in the rest table.
trans_rest_set_status(ResourceId, Status, Outcome) ->
    Q = qlc:q([X || X <- mnesia:table(rest), X#rest.resourceId == ResourceId]),
    Rows = qlc:e(Q),
    Timestamp = uuid:get_v1_time(),
    case Rows of
	[Row | []] ->
	    mnesia:write(Row#rest{status=Status, timestamp=Timestamp, outcome=Outcome}),
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
		    lists:foreach(fun(ResourceId) -> trans_rest_set_status(ResourceId, aborted, none) end, ResourceIds)
	    end,
    {atomic, ok} = mnesia:transaction(Trans),
    ok.

% Create REST resource (order)
rest_create(Facility, EidValue, Order) ->
    ResourceId = uuid:uuid_to_string(uuid:get_v4_urandom()),
    Timestamp = uuid:get_v1_time(),
    Row = #rest{resourceId=ResourceId, facility=Facility, eidValue=EidValue, order=Order, status=new,
		timestamp=Timestamp, outcome=none},
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
		    Q = qlc:q([{X#rest.status, X#rest.timestamp, X#rest.eidValue, X#rest.order, X#rest.outcome} ||
				  X <- mnesia:table(rest),
				  X#rest.resourceId == ResourceId, X#rest.facility == Facility]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
	[{Status, Timestamp, EidValue, Order, Outcome}] ->
	    {Status, Timestamp, EidValue, Order, Outcome};
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
			    WorkRow = #work{pid=Pid, resourceId=Row#rest.resourceId, state=none, order=Row#rest.order},
			    case mnesia:write(WorkRow) of
				ok ->
				    % We are now working on this order
				    ok = trans_rest_set_status(Row#rest.resourceId, work, none),
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

    {atomic , Result} = mnesia:transaction(Trans),
    case Result of
	{rest, _, Facility, _, Order, _, _, _} ->
	    logger:notice("Work: fetching new work item: EidValue=~p, Pid=~p, Order=~p, Facility=~p", [EidValue, Pid, Order, Facility]),
	    {Facility, Order};
	none ->
	    logger:notice("Work: no work item in database: EidValue=~p, Pid=~p", [EidValue, Pid]),
	    none;
	_ ->
	    logger:error("Work: cannot fetch work item, database error: EidValue=~p, Pid=~p", [EidValue, Pid]),
	    error
	end.

% Pickup a work item. This function can be called any time after work_fetch was called before.
work_pickup(Pid) ->
    Trans = fun() ->
		    Q = qlc:q([{X#work.order, X#work.state} || X <- mnesia:table(work), X#work.pid == Pid]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
	[{Order, State} | _] ->
	    {Order, State};
	[] ->
	    logger:error("Work: no work item found under specified Pid, already finished?, not fetched?: Pid=~p", [Pid]),
	    none;
	_ ->
	    logger:error("Work: cannot pick up work item, database error: Pid=~p", [Pid]),
	    error
    end.

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
work_finish(Pid, Status, Outcome) ->
    Trans = fun() ->
		    Q = qlc:q([X#work.resourceId || X <- mnesia:table(work), X#work.pid == Pid]),
		    Rows = qlc:e(Q),
		    case Rows of
			[] ->
			    error;
			[ResourceId | _] ->
			    Oid = {work, ResourceId},
			    mnesia:delete(Oid),
			    ok = trans_rest_set_status(ResourceId, Status, Outcome)
		    end
	    end,

    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
        ok ->
	    logger:notice("Work: finishing work item: Pid=~p, Status=~p, Outcome=~p", [Pid, Status, Outcome]),
	    ok;
	_ ->
	    logger:error("Work: cannot finish work item, database error: Pid=~p, Status=~p, Outcome=~p", [Pid, Status, Outcome]),
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

