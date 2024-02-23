% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(mnesia_db).
-include_lib("stdlib/include/qlc.hrl").
-export([setup/0, rest_list/1, rest_lookup/2, rest_create/3, rest_delete/2]).
-record(rest, {resourceId, facility, eidValue, order, status, timestamp}).

% Initialize databse scheme and tables
% (this is a setup routine that has to be executed only once to setup the database )
setup() ->
    io:format("stopping MNESIA...~n"),
    mnesia:stop(),
    io:format("creating schema...~n"),
    mnesia:create_schema([node()], disc_copies),
    io:format("starting MNESIA...~n"),
    mnesia:start(),
    io:format("enabeling disc_copies...~n"),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    io:format("creating REST table...~n"),
    mnesia:start(),
    mnesia:create_table(rest,
			[{attributes, record_info(fields, rest)},
			 {disc_copies, [node()]},
			 {type, set}
			]).

% Create REST resource
rest_create(Facility, EidValue, Order) ->
    ResourceId = uuid:uuid_to_string(uuid:get_v4_urandom()),
    Timestamp = uuid:get_v1_time(),
    Row = #rest{resourceId=ResourceId, facility=Facility, eidValue=EidValue, order=Order, status=new,
		timestamp=Timestamp},
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

% Lookup REST resource
rest_lookup(ResourceId, Facility) ->
    Trans = fun() ->
		    Q = qlc:q([{X#rest.status, X#rest.timestamp, X#rest.eidValue, X#rest.order} ||
				  X <- mnesia:table(rest),
				  X#rest.resourceId == ResourceId, X#rest.facility == Facility]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    case Result of
	[{Status, Timestamp, EidValue, Order}] ->
	    {Status, Timestamp, EidValue, Order};
	[] ->
	    none;
	_ ->
	    Result
    end.

% Delete REST resource
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

% List the resource identifiers of currently pending REST resources
rest_list(Facility) ->
    mnesia:start(),
    Trans = fun() ->
		    Q = qlc:q([X#rest.resourceId || X <- mnesia:table(rest), X#rest.facility == Facility]),
		    qlc:e(Q)
	    end,
    {atomic, Result} = mnesia:transaction(Trans),
    Result.





