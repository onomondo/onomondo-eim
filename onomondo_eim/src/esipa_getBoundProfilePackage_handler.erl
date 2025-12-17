% Copyright (c) 2025 Onomondo ApS & sysmocom - s.f.m.c. GmbH. All rights reserved.
%
% SPDX-License-Identifier: AGPL-3.0-only
%
% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_getBoundProfilePackage_handler).
-behavior(cowboy_handler).

-export([init/2]).

init(Req, State) ->
	{ok, Req, State}.
