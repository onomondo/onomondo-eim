% Implementation of the SGP.22 ES9+ Interface Client
%
% Applications are expected to make use of the high-level request_{json,asn1} functions in order to make ES9+
% requests to a SM-DP+.

-module(es9p_client).

-export([request_json/2, request_asn1/2, tryme/0]).

% send an ES9+ HTTP request in JSON format over HTTP binding
make_req_json(BaseUrl, Function, JsonBody) ->
    % see SGP.22 Section 6.5 for details on HTTP Function Binding in JSON
    Method = post,
    % construct URL from global hostname, static path and function
    URL = list_to_binary(string:join(["https://", binary_to_list(BaseUrl), "/gsma/rsp2/es9plus/", Function], "")),
    ReqHeaders = [ {<<"Content-Type">>, <<"application/json;charset=UTF-8">>},
		   {<<"X-Admin-Protocol">>, <<"gsma/rsp/v2.1.0">>} ],
    % Add a request header (see also: GSMA SGP.22 6.5.1.3)
    {ok, Hostname} = inet:gethostname(),
    JsonBodyWithHdr = JsonBody#{<<"header">> => #{<<"functionRequesterIdentifier">> => list_to_binary(Hostname),
						  <<"functionCallIdentifier">> => list_to_binary(pid_to_list(self()))}},
    % construct body from encoded json
    ReqBody = jiffy:encode(JsonBodyWithHdr, [force_utf8]),
    % TODO: actually verify the certificate by providing custom root CA Cert
    SslOptions = [ {verify, verify_none} ],
    Options = [ {ssl_options, SslOptions}, with_body ],
    io:format("Tx ES9+ JSON:~n"),
    io:format("headers: ~p~n", [ReqHeaders]),
    io:format("body: ~p~n", [ReqBody]),
    {ok, StatusCode, RespHeaders, RespBody} = hackney:request(Method, URL, ReqHeaders, ReqBody, Options),
    io:format("Rx ES9+ JSON:~n"),
    io:format("status: ~p~n", [StatusCode]),
    io:format("headers: ~p~n", [RespHeaders]),
    io:format("body: ~p~n", [RespBody]),
    % TODO: verify RespHeaders: X-Admin-Protocol
    % TODO: verify RespHeaders: Content-Type
    RespBodyDecoded = case StatusCode of
			  200 -> jiffy:decode(RespBody, [return_maps]);
			  _ -> <<>>
		      end,
    {ok, StatusCode, RespBodyDecoded}.

% send an ES9+ HTTP request in ASN.1 format over HTTP binding
make_req_asn1(BaseUrl, Asn1Body) ->
    % see SGP.22 Section 6.6 for details on HTTP Function Binding in ASN.1
    Method = post,
    % construct URL from global hostname, static path and function
    URL = list_to_binary(string:join([BaseUrl, "/gsma/rsp2/asn1"], "")),
    ReqHeaders = [ {<<"Content-Type">>, <<"application/x-gsma-rsp-asn1">>},
		   {<<"X-Admin-Protocol">>, <<"gsma/rsp/v2.1.0">>} ],
    % construct body from encoded ASN.1
    {ok, ReqBody} = 'RSPDefinitions':encode('RemoteProfileProvisioningRequest', Asn1Body),
    % TODO: actually verify the certificate by providing custom root CA Cert
    SslOptions = [ {verify, verify_none} ],
    Options = [ {ssl_options, SslOptions}, with_body ],
    io:format("Tx ES9+ ASN.1:~n"),
    io:format("headers: ~p~n", [ReqHeaders]),
    io:format("body: ~p~n", [ReqBody]),
    {ok, StatusCode, RespHeaders, RespBody} = hackney:request(Method, URL, ReqHeaders, ReqBody, Options),
    io:format("Rx ES9+ ASN.1:~n"),
    io:format("status: ~p~n", [StatusCode]),
    io:format("headers: ~p~n", [RespHeaders]),
    io:format("body: ~p~n", [RespBody]),
    % TODO: verify RespHeaders: X-Admin-Protocol
    % TODO: verify RespHeaders: Content-Type
    RespBodyDecoded = case StatusCode of
			  200 -> {ok, Asn1Decoded} = 'RSPDefinitions':decode('RemoteProfileProvisioningResponse', RespBody),
				 Asn1Decoded;
			  _ -> <<>>
		      end,
    {ok, StatusCode, RespBodyDecoded}.

% encode RSP ASN.1 data of given type + base64-encode it
rsp_enc_asn1_b64(TypeName, Data) ->
    {ok, Bin} = 'RSPDefinitions':encode(TypeName, Data),
    base64:encode(Bin).

% base64-decode and RSP ASN.1 decode data of given type
rsp_dec_b64_asn1(TypeName, Data) ->
    {ok, Dec} = 'RSPDefinitions':decode(TypeName, base64:decode(Data)),
    Dec.
pki_dec_b64_asn1(TypeName, Data) ->
    {ok, Dec} = 'PKIX1Explicit88':decode(TypeName, base64:decode(Data)),
    Dec.

% convert from RemoteProfileProvisioningRequest to weird ASN.1-base64-in-JSON
%
% Our philosophy here is to use the maps (ES9) genreated by the asn1ct compiler on the input of the request functions
% and craft the return values so that they use those maps as well. Eventually that means that the return values
% should look like if they were the output of the ASN.1 decoder. This makes sure that we can use the same decoded
% request/response structures for both the JSON and the ASN.1 binding of the ES9+ HTTP interface.

% GSMA SGP.22, section 6.5.2.6 and section 6.6.2.1
request_json({initiateAuthenticationRequest, InitAuthReq}, BaseUrl) ->
    Json = #{<<"euiccChallenge">> => base64:encode(maps:get(euiccChallenge, InitAuthReq)),
	     <<"smdpAddress">> => maps:get(smdpAddress, InitAuthReq),
	     <<"euiccInfo1">> => rsp_enc_asn1_b64('EUICCInfo1', maps:get(euiccInfo1, InitAuthReq))},

    {ok, _HtppStatus, JsonResp} = make_req_json(BaseUrl, "initiateAuthentication", Json),
    Choice = case JsonResp of
		 #{<<"header">> := #{<<"functionExecutionStatus">> := #{<<"status">> := <<"Executed-Success">>}}} ->
		     R = #{transactionId => utils:hex_to_binary(maps:get(<<"transactionId">>, JsonResp)),
			   serverSigned1 => rsp_dec_b64_asn1('ServerSigned1', maps:get(<<"serverSigned1">>, JsonResp)),
			   serverSignature1 => base64:decode(maps:get(<<"serverSignature1">>, JsonResp)),
			   euiccCiPKIdToBeUsed => base64:decode(maps:get(<<"euiccCiPKIdToBeUsed">>, JsonResp)),
			   serverCertificate => pki_dec_b64_asn1('Certificate', maps:get(<<"serverCertificate">>, JsonResp))},
		     {initiateAuthenticationOk, R};
		 _ ->
		     {initiateAuthenticationError, 127}
		 % TODO: At the moment we only react on "Executed-Success", we should also react on the other status
		 % codes and return an apropriate tuple (in JSON the concept of xyzOk/xyzError members
		 % like we have it in the ASN.1 definition seems not to exist), see also SGP.22, section 6.5.1.4.
	     end,
    {initiateAuthenticationResponse, Choice};

% GSMA SGP.22, section 6.5.2.8 and section 6.6.2.2
request_json({authenticateClientRequest, AuthClientReq}, BaseUrl) ->
    Json = #{<<"transactionId">> => utils:binary_to_hex(maps:get(transactionId, AuthClientReq)),
	     <<"authenticateServerResponse">> => rsp_enc_asn1_b64('AuthenticateServerResponse',
								  maps:get(authenticateServerResponse, AuthClientReq))},
    {ok, _HtppStatus, JsonResp} = make_req_json(BaseUrl, "authenticateClient", Json),
    Choice = case JsonResp of
		 #{<<"header">> := #{<<"functionExecutionStatus">> := #{<<"status">> := <<"Executed-Success">>}}} ->
		     R = #{transactionId => utils:hex_to_binary(maps:get(<<"transactionId">>, JsonResp)),
			   profileMetaData => rsp_dec_b64_asn1('StoreMetadataRequest', maps:get(<<"profileMetadata">>, JsonResp)),
			   smdpSigned2 => rsp_dec_b64_asn1('SmdpSigned2', maps:get(<<"smdpSigned2">>, JsonResp)),
			   smdpSignature2 => base64:decode(maps:get(<<"smdpSignature2">>, JsonResp)),
			   smdpCertificate => pki_dec_b64_asn1('Certificate', maps:get(<<"smdpCertificate">>, JsonResp))},
		     {authenticateClientOk, R};
		 _ ->
		     {authenticateClientError, 127}
		     % TODO: (see above)
	     end,
    {authenticateClientResponseEs9, Choice};

% GSMA SGP.22, section 6.5.2.7 and section 6.6.2.3
request_json({getBoundProfilePackageRequest, GetBppReq}, BaseUrl) ->
    Json = #{<<"transactionId">> => utils:binary_to_hex(maps:get(transactionId, GetBppReq)),
	     <<"prepareDownloadResponse">> => rsp_enc_asn1_b64('PrepareDownloadResponse', maps:get(prepareDownloadResponse, GetBppReq))},
    {ok, _HtppStatus, JsonResp} = make_req_json(BaseUrl, "getBoundProfilePackage", Json),
    Choice = case JsonResp of
		 #{<<"header">> := #{<<"functionExecutionStatus">> := #{<<"status">> := <<"Executed-Success">>}}} ->
		     R = #{transactionId => utils:hex_to_binary(maps:get(<<"transactionId">>, JsonResp)),
			   boundProfilePackage => rsp_dec_b64_asn1('BoundProfilePackage', maps:get(<<"boundProfilePackage">>, JsonResp))},
		     {getBoundProfilePackageOk, R};
		 _ ->
		     {getBoundProfilePackageError, 127}
		% TODO: (see above)
	     end,
    {getBoundProfilePackageResponse, Choice};

% GSMA SGP.22, section 6.5.2.10 and section 6.6.2.5
request_json({cancelSessionRequestEs9, CancelSessReq}, BaseUrl) ->
    Json = #{<<"transactionId">> => utils:binary_to_hex(maps:get(transactionId, CancelSessReq)),
	     <<"cancelSessionResponse">> => rsp_enc_asn1_b64('CancelSessionResponse', maps:get(cancelSessionResponse, CancelSessReq))},
    {ok, _HtppStatus, JsonResp} = make_req_json(BaseUrl, "cancelSession", Json),
    Choice = case JsonResp of
		 #{<<"header">> := #{<<"functionExecutionStatus">> := #{<<"status">> := <<"Executed-Success">>}}} ->
		     % This function has no output data
		     {cancelSessionOk, none};
		 _ ->
		     {cancelSessionError, 127}
		     % TODO: (see above)
	     end,
    {cancelSessionResponseEs9, Choice};

% GSMA SGP.22, section 6.5.2.9 and section 6.6.2.4
request_json({handleNotification, HandleNotifReq}, BaseUrl) ->
    Json = #{<<"pendingNotification">> => rsp_enc_asn1_b64('PendingNotification', maps:get(pendingNotification, HandleNotifReq))},
    {ok, _HtppStatus, _JsonResp} = make_req_json(BaseUrl, "handleNotification", Json),
    % TODO: check HTTP status?
    % There is no response defined for this function (see also SGP.22, section 5.6.4), so we send just forward an
    % an empty tuple.
    {}.

% issue a RemoteProfileProvisioningRequest in its ASN.1 form
% (See also SGP.22 6.6.2.1 - 6.6.2.5)
request_asn1(RemPpr, BaseUrl) ->
    {ok, _HttpStatus, Asn1Resp} = make_req_asn1(BaseUrl, RemPpr),
    % TODO: check HTTP status?
    Asn1Resp.

tryme() ->
    Req = {initiateAuthenticationRequest,#{euiccChallenge =>
					       <<98,247,104,103,113,183,83,201,207,31,83,
						 25,64,67,206,171>>,
					   euiccInfo1 =>
					       #{euiccCiPKIdListForSigning =>
						     [<<245,65,114,189,249,138,149,214,
							92,190,184,138,56,161,193,29,128,
							10,133,195>>,
						      <<192,188,112,186,54,146,157,67,
							180,103,255,87,87,5,48,229,122,
							184,252,216>>],
						 euiccCiPKIdListForVerification =>
						     [<<245,65,114,189,249,138,149,214,
							92,190,184,138,56,161,193,29,128,
							10,133,195>>,
						      <<192,188,112,186,54,146,157,67,
							180,103,255,87,87,5,48,229,122,
							184,252,216>>],
						 svn => <<2,3,0>>},
					   smdpAddress => <<"127.0.0.1">>}},
    request_json(Req, <<"127.0.0.1:4430">>).
