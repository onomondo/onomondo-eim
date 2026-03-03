-module(esipa_asn1_handler_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([get_eim_package_no_pending/1]).

all() ->
    [get_eim_package_no_pending].

init_per_suite(Config) ->
    ok = mnesia_db:init(),
    Config.

end_per_suite(_Config) ->
    _ = mnesia:stop(),
    ok.

get_eim_package_no_pending(_Config) ->
    EidValue = <<16, 50, 84, 118, 152, 186, 220, 254, 239, 205, 171, 137, 103, 69, 35, 1>>,
    Response = esipa_asn1_handler:handle_asn1(
        self(),
        {getEimPackageRequest, #{eidValue => EidValue}}
    ),
    Expected = {getEimPackageResponse, {eimPackageError, noEimPackageAvailable}},
    ?assertEqual(Expected, Response).
