% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(onomondo_eim_app).
-behaviour(application).
-export([start/2, esipa_dispatch/0]).
-export([stop/1]).

start_esipa_server(true, Ip, Port, _Cert, _Key, Dispatch) ->
    logger:notice("Starting ESipa HTTP server at ~p:~p...~n", [Ip, Port]),
    cowboy:start_clear(
        http_listener_esipa,
        [
            {ip, Ip},
            {port, Port}
        ],
        #{
            env => #{
                dispatch => Dispatch
            },
            middlewares => [cowboy_router, esipa_middleware, cowboy_handler]
        }
    );
start_esipa_server(false, Ip, Port, Cert, Key, Dispatch) ->
    logger:notice(
        "Starting ESipa HTTPs server at ~p:~p...~ncertificate: ~p~nkey: ~p~n",
        [Ip, Port, Cert, Key]
    ),
    cowboy:start_tls(
        https_listener_esipa,
        [
            {ip, Ip},
            {port, Port},
            {certfile, Cert},
            {keyfile, Key}
        ],
        #{
            env => #{
                dispatch => Dispatch
            },
            middlewares => [cowboy_router, esipa_middleware, cowboy_handler]
        }
    ).

esipa_dispatch() ->
    cowboy_router:compile([
        {'_', [
            % SGP.32 Section 6.4.1
            {"/gsma/rsp2/esipa/[...]", esipa_json_handler, []},
            % SGP.32 Section 6.1.1: Any function execution request using ASN.1 binding SHALL be sent to the generic
            % HTTP path 'gsma/rsp2/asn1'
            {"/gsma/rsp2/asn1", esipa_asn1_http_handler, []},
            % MISC
            {"/", esipa_infopage, []}
        ]}
    ]).

rest_dispatch() ->
    cowboy_router:compile([
        {'_', [
            % Downloads
            {"/download/create/", rest_handler, [download, create]},
            {"/download/lookup/:resource_id", rest_handler, [download, lookup]},
            {"/download/delete/:resource_id", rest_handler, [download, delete]},
            {"/download/list/", rest_handler, [download, list]},
            % PSMO
            {"/psmo/create/", rest_handler, [psmo, create]},
            {"/psmo/lookup/:resource_id", rest_handler, [psmo, lookup]},
            {"/psmo/delete/:resource_id", rest_handler, [psmo, delete]},
            {"/psmo/list/", rest_handler, [psmo, list]},
            % ECO
            {"/eco/create/", rest_handler, [eco, create]},
            {"/eco/lookup/:resource_id", rest_handler, [eco, lookup]},
            {"/eco/delete/:resource_id", rest_handler, [eco, delete]},
            {"/eco/list/", rest_handler, [eco, list]},
            % EDR
            {"/edr/create/", rest_handler, [edr, create]},
            {"/edr/lookup/:resource_id", rest_handler, [edr, lookup]},
            {"/edr/delete/:resource_id", rest_handler, [edr, delete]},
            {"/edr/list/", rest_handler, [edr, list]},
            % EUICC
            {"/euicc/create/", rest_handler, [euicc, create]},
            {"/euicc/lookup/:resource_id", rest_handler, [euicc, lookup]},
            {"/euicc/delete/:resource_id", rest_handler, [euicc, delete]},
            {"/euicc/list/", rest_handler, [euicc, list]},
            % MISC
            {"/", rest_handler, [info]}
        ]}
    ]).

start(_Type, _Args) ->
    {ok, Vsn} = application:get_key(onomondo_eim, vsn),
    logger:notice("eIM! version:~s~n", [Vsn]),

    % Startup database
    ok = mnesia_db:init(),

    % Startup ESipa server
    Dispatch_ESipa = esipa_dispatch(),
    {ok, EsipaIp} = application:get_env(onomondo_eim, esipa_ip),
    {ok, EsipaPort} = application:get_env(onomondo_eim, esipa_port),
    {ok, EsipaSslDisable} = application:get_env(onomondo_eim, esipa_ssl_disable),
    {ok, EsipaSslCert} = application:get_env(onomondo_eim, esipa_ssl_cert),
    {ok, EsipaSslKey} = application:get_env(onomondo_eim, esipa_ssl_key),
    {ok, _} = start_esipa_server(
        EsipaSslDisable,
        EsipaIp,
        EsipaPort,
        EsipaSslCert,
        EsipaSslKey,
        Dispatch_ESipa
    ),

    % Startup REST server
    Dispatch_REST = rest_dispatch(),
    {ok, RestIp} = application:get_env(onomondo_eim, rest_ip),
    {ok, RestPort} = application:get_env(onomondo_eim, rest_port),
    logger:notice("Starting REST HTTP server at ~p:~p...~n", [RestIp, RestPort]),
    {ok, _} = cowboy:start_clear(
        http_listener_rest,
        [
            {ip, RestIp},
            {port, RestPort}
        ],
        #{env => #{dispatch => Dispatch_REST}}
    ),

    onomondo_eim_sup:start_link().

stop(_State) ->
    ok.
