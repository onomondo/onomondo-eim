% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_getBoundProfilePackage_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    {ok, Req, State}.
