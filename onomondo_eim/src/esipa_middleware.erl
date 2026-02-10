% Copyright (c) 2025 Onomondo ApS & sysmocom - s.f.m.c. GmbH. All rights reserved.
%
% SPDX-License-Identifier: AGPL-3.0-only
%
% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module (esipa_middleware).
-behaviour(cowboy_middleware).

-export([execute/2]).

% "X-Admin-Protocol" header field SHALL be set to v2.1.0 in both HTTP request and HTTP response.
% Content-Type: "application/x-gsmsa-rsp-asn1" or "application/json;charset=UTF-8"

execute(Req0=#{method := <<"POST">>}, State) ->
	case cowboy_req:header(<<"x-admin-protocol">>, Req0) of
		<<"gsma/rsp/v2.1.0">> ->
			{ok, Req0, State};
		_ ->
			Req = cowboy_req:reply(400, <<"Unsupported x-admin-protocol">>, Req0),
			{ok, Req, State}
	end;
execute(Req0, Env) ->
	io:format("~p request~n", [cowboy_req:method(Req0)]),
	Req = cowboy_req:reply(405, #{ <<"allow">> => <<"POST">> }, Req0),
	{ok, Req, Env}.
