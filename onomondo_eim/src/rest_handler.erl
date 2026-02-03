% Copyright (c) 2025 Onomondo ApS & sysmocom - s.f.m.c. GmbH. All rights reserved.
%
% SPDX-License-Identifier: AGPL-3.0-only
%
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
				   get_rest_list(Req, State, Facility);
			       [info] ->
				   get_rest_info(Req, State)
			   end,
    {Body, Req1, State1}.

% Create a new resource and return its identifier
post_rest_create(Req, State, Facility) ->
    {ok, [{Content, true}], Req1} = cowboy_req:read_urlencoded_body(Req),
    logger:info("REST: creating new REST resource,~nPeer=~p, Resource=~p, Facility=~p",
		[maps:get(peer, Req), Content, Facility]),
    ContentDecoded = jiffy:decode(Content),
    {[{<<"eidValue">>, EidValue}, _]} = ContentDecoded,
    {[_, {<<"order">>, Order}]} = ContentDecoded,
    {ok, ResourceId} = mnesia_db:rest_create(Facility, EidValue, Order),
    case cowboy_req:method(Req1) of
        <<"POST">> ->
            Response = io_lib:format("/~s/lookup/~s", [Facility, ResourceId]),
	    logger:info("REST: responding to client,~nPeer=~p, ResourceId=~p, Response:~p~n",
			[maps:get(peer, Req), ResourceId, list_to_binary(Response)]),
            {{true, list_to_binary(Response)}, Req1, State};
        _ ->
            {true, Req1, State}
    end.

% Lookup a specific resource and output it to the requestor
get_rest_lookup(Req, State, Facility) ->
    ResourceId = binary_to_list((cowboy_req:binding(resource_id, Req))),
    logger:info("REST: client requests REST resource for lookup,~nPeer=~p, ResourceId=~p, Facility=~p~n",
		[maps:get(peer, Req), ResourceId, Facility]),
    Result = mnesia_db:rest_lookup(ResourceId, Facility),
    Response = case Result of
		   {Status, Timestamp, EidValue, Order, Outcome, Debuginfo} ->
		       % Here a "resource" refers to the parameters that the REST API user has originally submitted
		       % during create. We will only read the resource but not change it.
		       Resource = {[{eidValue, EidValue}, {order, Order}]},
		       DebuginfoHex = utils:binary_to_hex(term_to_binary(Debuginfo)),
		       TimestampStr = list_to_binary(io_lib:format("~p", [Timestamp])),
		       {[{status, Status},
			 {timestamp, TimestampStr},
			 {resource, Resource},
			 {outcome, Outcome},
			 {debuginfo, DebuginfoHex}]};
		   none ->
		       {[{status, absent}]};
		   _ ->
		       {[{status, error}]}
	       end,
    ResponseJson = jiffy:encode(Response),
    logger:info("REST: responding to client,~nPeer=~p, ResourceId=~p, Response:~p~n",
		[maps:get(peer, Req), ResourceId, ResponseJson]),
    {ResponseJson, Req, State}.

% Delete a specific resource
get_rest_delete(Req, State, Facility) ->
    ResourceId = binary_to_list((cowboy_req:binding(resource_id, Req))),
    logger:info("REST: client requests REST resource for delete,~nPeer=~p, ResourceId=~p, Facility=~p~n",
		[maps:get(peer, Req), ResourceId, Facility]),
    Result = mnesia_db:rest_delete(ResourceId, Facility),
    Response = case Result of
		   ok ->
		       {[{status, deleted}]};
		   none ->
		       {[{status, absent}]};
		   _ ->
		       {[{status, error}]}
	       end,
    ResponseJson = jiffy:encode(Response),
    logger:info("REST: responding to client,~nPeer=~p, ResourceId=~p, Response:~p~n",
		[maps:get(peer, Req), ResourceId, ResponseJson]),
    {ResponseJson, Req, State}.

% Output a list of all pending resources to the requestor
get_rest_list(Req, State, Facility) ->
    logger:info("REST: client requests REST resource list,~nPeer=~p, Facility=~p~n",
		[maps:get(peer, Req), Facility]),
    Result = mnesia_db:rest_list(Facility),
    ResourceIdList = [ list_to_binary(X) || X <- Result],
    Response = io_lib:format("{\"resourceIdList\": ~s}",
			     [binary_to_list(jiffy:encode(ResourceIdList))]),
    logger:info("REST: responding to client,~nPeer=~p, Response:~p",
		[maps:get(peer, Req), list_to_binary(Response)]),
    {list_to_binary(Response), Req, State}.

% Output a list with basic information about the eIM instance to the requestor
get_rest_info(Req, State) ->
    logger:info("REST: client requests info,~nPeer=~p~n", [maps:get(peer, Req)]),
    {ok, EimId} = application:get_env(onomondo_eim, eim_id),
    {ok, EsipaIp} = application:get_env(onomondo_eim, esipa_ip),
    {ok, EsipaPort} = application:get_env(onomondo_eim, esipa_port),
    {ok, Hostname} = inet:gethostname(),
    {ok, Vsn} = application:get_key(onomondo_eim, vsn),
    {ok, EsipaSslCertPath} = application:get_env(onomondo_eim, esipa_ssl_cert),
    {ok, EsipaSslCertPem} = file:read_file(EsipaSslCertPath),
    {ok, EimCertPath} = application:get_env(onomondo_eim, eim_cert),
    {ok, EimCertPem} = file:read_file(EimCertPath),
    {ok, CounterValue} = application:get_env(onomondo_eim, counter_value),
    {ok, ConsumerEuicc} = application:get_env(onomondo_eim, consumer_euicc),
    FormatRootCiCert = fun(RootCiCertPath) ->
			       {ok, RootCiCertPem} = file:read_file(RootCiCertPath),
			       RootCiCertPem
		       end,
    {ok, RootCiCerts} = application:get_env(onomondo_eim, root_ci_certs),
    RootCiCertsJson = [FormatRootCiCert(RootCiCertPath) || RootCiCertPath <- RootCiCerts],

    InfoList = {[
		  {hostname, list_to_binary(Hostname)},
		  {node, node()},
		  {vsn, list_to_binary(Vsn)},
		  {eimId, list_to_binary(EimId)},
		  {esipaIp, list_to_binary(inet:ntoa(EsipaIp))},
		  {esipaPort, EsipaPort},
		  {esipaSslCert, EsipaSslCertPem},
		  {eimCert, EimCertPem},
		  {rootCiCerts, RootCiCertsJson},
		  {addInitialEimRequest, eim_cfg:gen_eim_configuration_data(request)},
		  {eimConfigurationData, eim_cfg:gen_eim_configuration_data(single)},
		  {counterValue, CounterValue},
		  {consumerEuicc, ConsumerEuicc}
		]},
    InfoListJson = utils:join_binary_list(jiffy:encode(InfoList)),
    Response = io_lib:format("~s", [binary_to_list(InfoListJson)]),
    logger:info("REST: responding to client,~nPeer=~p, Response=~p~n",
		[maps:get(peer, Req), list_to_binary(Response)]),
    {list_to_binary(Response), Req, State}.
