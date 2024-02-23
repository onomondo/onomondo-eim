% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(onomondo_eim_app).
-behaviour(application).
-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    Dispatch_ESipa = cowboy_router:compile([
        {'_', [
	       % SGP.32 Section 6.4.1
	       {"/gsma/rsp2/esipa/initiateAuthentication", initiateAuthentication_handler, []},
	       {"/gsma/rsp2/esipa/authenticateClient", authenticateClient_handler, []},
	       {"/gsma/rsp2/esipa/getBoundProfilePackage", getBoundProfilePackage_handler, []},
               {"/gsma/rsp2/esipa/transferEimPackage", transferEimPackage_handler, []},
               {"/gsma/rsp2/esipa/getEimPackage", getEimPackage_handler, []},
               {"/gsma/rsp2/esipa/provideEimPackageResult", provideEimPackageresult_handler, []},
               {"/gsma/rsp2/esipa/handleNotification", handleNotification_handler, []},
               {"/gsma/rsp2/esipa/cancelSession", cancelSession_handler, []},
               % SGP.32 Section 6.1.1: Any function execution request using ASN.1 binding SHALL be sent to the generic HTTP path 'gsma/rsp2/asn1'
               {"/gsma/rsp2/asn1", asn1_handler, []}
              ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener_esipa,
				 [{port, 8000}], %TODO: make port user configurable
				 #{env => #{dispatch => Dispatch_ESipa,
					    middlewares => [cowboy_router, eim_esipa_middleware, cowboy_handler]}}
				),

    Dispatch_REST = cowboy_router:compile([
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
               {"/eco/list/", rest_handler, [eco, list]}
              ]}
    ]),
    {ok, _} = cowboy:start_clear(http_listener_rest,
				 [{port, 8080}], %TODO: make port user configurable
				 #{env => #{dispatch => Dispatch_REST}}
				),

    onomondo_eim_sup:start_link().

stop(_State) ->
	ok.
