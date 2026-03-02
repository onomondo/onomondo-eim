-module(esipa_json_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req0, State) ->
    Path = cowboy_req:path(Req0),
    Req =
        case is_esipa_endpoint(Path) of
            true ->
                cowboy_req:reply(
                    501,
                    #{
                        <<"content-type">> => <<"application/json">>
                    },
                    <<"{}">>,
                    Req0
                );
            false ->
                cowboy_req:reply(
                    404,
                    #{
                        <<"content-type">> => <<"text/plain">>
                    },
                    <<"Not Found">>,
                    Req0
                )
        end,
    {ok, Req, State}.

is_esipa_endpoint(Path) ->
    lists:member(
        Path,
        [
            <<"/gsma/rsp2/esipa/initiateAuthentication">>,
            <<"/gsma/rsp2/esipa/authenticateClient">>,
            <<"/gsma/rsp2/esipa/getBoundProfilePackage">>,
            <<"/gsma/rsp2/esipa/transferEimPackage">>,
            <<"/gsma/rsp2/esipa/getEimPackage">>,
            <<"/gsma/rsp2/esipa/provideEimPackageResult">>,
            <<"/gsma/rsp2/esipa/handleNotification">>,
            <<"/gsma/rsp2/esipa/cancelSession">>
        ]
    ).
