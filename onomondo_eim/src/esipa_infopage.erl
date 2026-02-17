% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH
%
% This is testpage to allow for an easy conformation that the eIM is reachable.

-module(esipa_infopage).

-export([init/2]).

init(Req0, State) ->
    Req = cowboy_req:reply(
        200,
        #{
            <<"content-type">> => <<"text/plain">>
        },
        <<"eIM working!">>,
        Req0
    ),
    {ok, Req, State}.
