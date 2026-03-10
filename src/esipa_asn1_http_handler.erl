% Copyright (c) 2025 Onomondo ApS & sysmocom - s.f.m.c. GmbH. All rights reserved.
%
% SPDX-License-Identifier: AGPL-3.0-only
%
% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_asn1_http_handler).
-behavior(cowboy_handler).

-export([init/2, terminate/3]).

-define(RESPONSE_HEADERS, #{
    <<"content-type">> => <<"application/x-gsma-rsp-asn1">>,
    <<"x-admin-protocol">> => <<"gsma/rsp/v2.1.0">>
}).

% Process HTTP request
init(Req0, State) ->
    Req =
        case cowboy_req:header(<<"content-type">>, Req0) of
            <<"application/x-gsma-rsp-asn1">> ->
                % do the asn1 decode of the request body; dispatch to real handler
                {ok, Data, _Req1} = cowboy_req:read_body(Req0),
                {ok, IpaToEim} = esipa_asn1_codec:decode_ipa_to_eim(Data),
                {EsipaMsgType, _} = IpaToEim,
                logger:info(
                    "Handling incoming IPAd request: ~p,~nPeer=~p, Pid=~p~n",
                    [EsipaMsgType, maps:get(peer, Req0), maps:get(pid, Req0)]
                ),
                logger:debug(
                    "Rx ESipa ASN.1,~nPeer=~p, Pid=~p,~nIpaToEim=~p~n",
                    [maps:get(peer, Req0), maps:get(pid, Req0), IpaToEim]
                ),
                EimToIpa = esipa_asn1_handler:handle_asn1(maps:get(pid, Req0), IpaToEim),
                logger:debug(
                    "Tx ESipa ASN.1,~nPeer=~p,Pid=~p,~nEimToIpa=~p~n",
                    [maps:get(peer, Req0), maps:get(pid, Req0), EimToIpa]
                ),
                case EimToIpa of
                    {error, unsupported_request} ->
                        cowboy_req:reply(
                            400,
                            ?RESPONSE_HEADERS,
                            <<"Unsupported Request">>,
                            Req0
                        );
                    _ ->
                        {ok, EncodedRespBody} = esipa_asn1_codec:encode_eim_to_ipa(EimToIpa),
                        cowboy_req:reply(200, ?RESPONSE_HEADERS, EncodedRespBody, Req0)
                end;
            _ ->
                cowboy_req:reply(415, ?RESPONSE_HEADERS, <<"Unsupported content-type">>, Req0)
        end,
    {ok, Req, State}.

% Handle termination of HTTP requests
terminate(Reason, Req0, _State) ->
    case Reason of
        normal ->
            ok;
        _ ->
            mnesia_db:work_finish(
                maps:get(pid, Req0), [{[{procedureError, abortedOrder}]}], Reason
            ),
            logger:info(
                "Handling of IPAd request terminated unexpectedly, Reason=~p Pid=~p~n",
                [Reason, maps:get(pid, Req0)]
            ),
            cowboy_req:reply(500, ?RESPONSE_HEADERS, <<"Internal Server Error">>, Req0)
    end.
