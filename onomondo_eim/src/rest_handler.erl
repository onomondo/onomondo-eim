% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(rest_handler).

% Functions we provide to the Cowboy REST handler
-export([
	 init/2,
         allowed_methods/2,
         content_types_provided/2,
         content_types_accepted/2,
         handle_get_request/2,
         handle_post_request/2
        ]).

-record(state, {op}).

% Initiaize state and extract the operation (create, lookup, update, list)
init(Req, Options) ->
    io:format("Incoming REST request: ~p~n", [Options]),
    State = #state{op=Options},
    {cowboy_rest, Req, State}.

% Tell the Cowboy REST handler what HTTP methods we are able to perform
allowed_methods(Req, State) ->
    {[<<"GET">>, <<"POST">>], Req, State}.

% Tell the Cowboy REST handler what content types we provide
content_types_provided(Req, State) ->
    {[{<<"application/json">>, handle_get_request}], Req, State}.

% Tell the Cowboy REST handler what content types we accept
content_types_accepted(Req, State) ->
    {[{<<"application/json">>, handle_post_request}], Req, State}.

% Handle incoming POST requests (output input from requestor)
handle_post_request(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
			       [Facility, create] ->
				   post_rest_create(Req, State, Facility)
			   end,
    {Body, Req1, State1}.

% Handle incoming GET requests (output data to requestor)
handle_get_request(Req, #state{op=Op} = State) ->
    {Body, Req1, State1} = case Op of
			       [Facility, lookup] ->
				   get_rest_lookup(Req, State, Facility);
			       [Facility, delete] ->
				   get_rest_delete(Req, State, Facility);
			       [Facility, list] ->
				   get_rest_list(Req, State, Facility)
			   end,
    {Body, Req1, State1}.

% Create a new resource and return its identifier
post_rest_create(Req, State, Facility) ->
    {ok, [{Content, true}], Req1} = cowboy_req:read_urlencoded_body(Req),
    io:format("creating new REST resource: ~p~n", [Content]),
    ContentDecoded = jiffy:decode(Content),
    {[{<<"eidValue">>, EidValue}, _]} = ContentDecoded,
    {[_, {<<"order">>, Order}]} = ContentDecoded,
    {ok, ResourceId} = mnesia_db:rest_create(Facility, EidValue, Order),
    case cowboy_req:method(Req1) of
        <<"POST">> ->
            Response = io_lib:format("/~s/lookup/~s", [Facility, ResourceId]),
            {{true, list_to_binary(Response)}, Req1, State};
        _ ->
            {true, Req1, State}
    end.

% Lookup a specific resource and output it to the requestor
get_rest_lookup(Req, State, Facility) ->
    ResourceId = binary_to_list((cowboy_req:binding(resource_id, Req))),
    io:format("retrieving REST resource: ~p:~n", [ResourceId]),
    Result = mnesia_db:rest_lookup(ResourceId, Facility),
    Response = case Result of
		   {Status, Timestamp, EidValue, Order} ->
		       Resource = {[ {<<"eidValue">>, EidValue}, {<<"order">>, Order}]},
		       io_lib:format("{\"status\": \"~p\", \"timestamp\": \"~p\", \"resource\": ~s}",
				     [Status, Timestamp, binary_to_list(jiffy:encode(Resource))]);
		   none -> "{\"status\": \"absent\"}";
		   _ -> "{\"status\": \"error\"}"
	       end,
    {list_to_binary(Response), Req, State}.

% Delete a specific resource
get_rest_delete(Req, State, Facility) ->
    ResourceId = binary_to_list((cowboy_req:binding(resource_id, Req))),
    io:format("deleting REST resource: ~p:~n", [ResourceId]),
    Result = mnesia_db:rest_delete(ResourceId, Facility),
    Response = case Result of
		   ok -> "{\"status\": \"deleted\"}";
		   none -> "{\"status\": \"absent\"}";
		   _ -> "{\"status\": \"error\"}"
	       end,
    {list_to_binary(Response), Req, State}.

% Output a list of all pending resources to the requestor
get_rest_list(Req, State, Facility) ->
    io:format("retrieving REST resource list~n"),
    Result = mnesia_db:rest_list(Facility),
    ResourceIdList = [ list_to_binary(X) || X <- Result],
    Response = io_lib:format("{\"resource_id_list\": ~s}",
			     [binary_to_list(jiffy:encode(ResourceIdList))]),
    {list_to_binary(Response), Req, State}.
