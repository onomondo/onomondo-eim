-module(onomondo_eim_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([eim_info_endpoint/1, esipa_rejects_missing_protocol/1]).

all() ->
    [eim_info_endpoint, esipa_rejects_missing_protocol].

init_per_suite(Config) ->
    application:ensure_all_started(inets),
    application:ensure_all_started(cowboy),
    Dispatch = onomondo_eim_app:esipa_dispatch(),
    {ok, _} = cowboy:start_clear(
        http_listener_tests,
        [
            {ip, {127, 0, 0, 1}},
            {port, 0}
        ],
        #{
            env => #{
                dispatch => Dispatch
            },
            middlewares => [cowboy_router, esipa_middleware, cowboy_handler]
        }
    ),
    Port = ranch:get_port(http_listener_tests),
    [{port, Port} | Config].

end_per_suite(_Config) ->
    ok = cowboy:stop_listener(http_listener_tests).

eim_info_endpoint(Config) ->
    Url = lists:flatten(io_lib:format("http://127.0.0.1:~p/", [proplists:get_value(port, Config)])),
    {ok, {{_, _Status, _}, _Headers, Body}} = httpc:request(
        get,
        {Url, []},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual(<<"eIM working!">>, Body).

esipa_rejects_missing_protocol(Config) ->
    Url = lists:flatten(
        io_lib:format("http://127.0.0.1:~p/gsma/rsp2/asn1", [proplists:get_value(port, Config)])
    ),
    {ok, {{_, Status, _}, _Headers, Body}} = httpc:request(
        post,
        {Url, [], "application/x-gsma-rsp-asn1", <<>>},
        [],
        [{body_format, binary}]
    ),
    ?assertEqual(400, Status),
    ?assertEqual(<<"Unsupported x-admin-protocol">>, Body).
