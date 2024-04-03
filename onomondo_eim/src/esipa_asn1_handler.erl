% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_asn1_handler).
-behavior(cowboy_handler).

-export([init/2]).

-define(RESPONSE_HEADERS, #{<<"content-type">> => <<"application/x-gsma-rsp-asn1">>,
			    <<"x-admin-protocol">> => <<"gsma/rsp/v2.1.0">>}).

%GSMA SGP.32, section 6.3.2.1
handle_asn1(Req0, _State, {initiateAuthenticationRequestEsipa, EsipaReq}) ->
    % TODO: insert smdpAddress and/or eUICCInfo1 are optional fields in ESipa, which means they may be absent.
    % However in ES9+ those fields are mandatory. This means we may need to fill in those fields here, from cached
    % values.

    {_, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0)),
    BaseUrl = maps:get(smdpAddress, EsipaReq),
    NewWorkState = WorkState#{smdpAddress => BaseUrl},
    mnesia_db:work_update(maps:get(pid, Req0), NewWorkState),

    % setup ES9+ request message
    Es9Req = {initiateAuthenticationRequest, EsipaReq},

    % perform ES9+ request
    {initiateAuthenticationResponse, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp = case Es9Resp of
		    {initiateAuthenticationOk, InitAuthOk} ->
			TransactionId = maps:get(transactionId, InitAuthOk),
			io:format("====TransactionId===>~p~n", [TransactionId]),
                        % TODO: InitiateAuthenticationOkEsipa has matchingId and ctxPrams1 as optional members. In case
                        % we are able to populate those fields from cached values, we should do so.
                        % maps:merge(InitAuthOk, #{matchingId => FIXME, ctxPrams1 => FIXME}),
			{initiateAuthenticationOkEsipa, InitAuthOk};
		    {initiateAuthenticationError, InitAuthErr} ->
			{initiateAuthenticationErrorEsipa, InitAuthErr}
		end,
    {initiateAuthenticationResponseEsipa, EsipaResp};

%GSMA SGP.32, section 6.3.2.2
handle_asn1(Req0, _State, {authenticateClientRequestEsipa, EsipaReq}) ->
    {_, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0)),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    AuthServResp = maps:get(authenticateServerResponse, EsipaReq),
    Es9Req = case AuthServResp of
		 {authenticateResponseOk, AuthRespOk} ->
		     TransactionId = maps:get(transactionId, EsipaReq),
		     io:format("====TransactionId===>~p~n", [TransactionId]),
		     {authenticateClientRequest,
		      #{transactionId => maps:get(transactionId, EsipaReq),
			authenticateServerResponse => {authenticateResponseOk, AuthRespOk}}};
		 {authenticateResponseError, AuthRespErr} ->
		     TransactionId = maps:get(transactionId, EsipaReq),
		     io:format("====TransactionId===>~p~n", [TransactionId]),
		     {authenticateClientRequest,
		      #{transactionId => maps:get(transactionId, EsipaReq),
			authenticateServerResponse => {authenticateResponseError, AuthRespErr}}};
		 {compactAuthenticateResponseOk, _compartAuthRespOk} ->
		     throw("IPA Capability \"minimizeEsipaBytes\" (optional) not supported by this eIM")
	     end,

    % perform ES9+ request
    {authenticateClientResponseEs9, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp = case Es9Resp of
		    {authenticateClientOk, AuthClntRespEs9} ->
			{authenticateClientOkDPEsipa, AuthClntRespEs9};
		    {authenticateClientError, AuthClntErr} ->
			{authenticateClientErrorEsipa, AuthClntErr}
		end,
    {authenticateClientResponseEsipa, EsipaResp};

%GSMA SGP.32, section 6.3.2.3
handle_asn1(Req0, _State, {getBoundProfilePackageRequestEsipa, EsipaReq}) ->
    {_, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0)),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    PrepDwnldResp = maps:get(prepareDownloadResponse, EsipaReq),
    Es9Req = case PrepDwnldResp of
		 {downloadResponseOk, DwnldRespOk} ->
		     TransactionId = maps:get(transactionId, EsipaReq),
		     io:format("====TransactionId===>~p~n", [TransactionId]),
		     {getBoundProfilePackageRequest,
		      #{transactionId => maps:get(transactionId, EsipaReq),
			prepareDownloadResponse => {downloadResponseOk, DwnldRespOk}}};
		 {downloadResponseError, DwnldRespErr} ->
		     TransactionId = maps:get(transactionId, EsipaReq),
		     io:format("====TransactionId===>~p~n", [TransactionId]),
		     {getBoundProfilePackageRequest,
		      #{transactionId => maps:get(transactionId, EsipaReq),
			prepareDownloadResponse => {downloadResponseError, DwnldRespErr}}};
		 {compactDownloadResponseOk, _CompactAuthRespOk} ->
		     throw("IPA Capability \"minimizeEsipaBytes\" (optional) not supported by this eIM")
	     end,
    
    % perform ES9+ request
    {getBoundProfilePackageResponse, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp = case Es9Resp of
		    {getBoundProfilePackageOk, GetBndPrflePkgOk} ->
			% (in case IPA Capability 'minimizeEsipaBytes' is used, the transactionId has to be removed.)
			{getBoundProfilePackageOkEsipa, GetBndPrflePkgOk};
		    {getBoundProfilePackageError, GetBndPrflePkgErr} ->
			{getBoundProfilePackageErrorEsipa, GetBndPrflePkgErr}
		end,
    {getBoundProfilePackageResponseEsipa, EsipaResp};

%GSMA SGP.32, section 6.3.2.5
handle_asn1(Req0, _State, {cancelSessionRequestEsipa, EsipaReq}) ->
    {_, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0)),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    CancelSessionReq = maps:get(cancelSessionResponse, EsipaReq),
    Es9Req = case CancelSessionReq of
		 {cancelSessionResponseOk, CancelSessionRespOk} ->
		     TransactionId = maps:get(transactionId, EsipaReq),
		     io:format("====TransactionId===>~p~n", [TransactionId]),
		     {cancelSessionRequestEs9,
		      #{transactionId => maps:get(transactionId, EsipaReq),
			cancelSessionResponse => {cancelSessionResponseOk, CancelSessionRespOk}}};
		 {cancelSessionResponseError, CancelSessionRespErr} ->
		     TransactionId = maps:get(transactionId, EsipaReq),
		     io:format("====TransactionId===>~p~n", [TransactionId]),
		     {cancelSessionRequestEs9,
		      #{transactionId => maps:get(transactionId, EsipaReq),
			cancelSessionResponse => {cancelSessionResponseError, CancelSessionRespErr}}};
		 {compactCancelSessionResponseOk, _CompactCancelSessionReq} ->
		     throw("IPA Capability \"minimizeEsipaBytes\" (optional) not supported by this eIM")
	     end,

    % perform ES9+ request
    {cancelSessionResponseEs9, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    % CancelSessionResponseEsipa and CancelSessionResponseEs9 share the exact same definition, so we may convert
    % without an extra case statement.

    ok = mnesia_db:work_finish(maps:get(pid, Req0), canceled, EsipaReq),
    {cancelSessionResponseEsipa, Es9Resp};

%GSMA SGP.32, section 6.3.2.4
handle_asn1(Req0, _State, {handleNotificationEsipa, EsipaReq}) ->
    {_, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0)),
    BaseUrl = maps:get(smdpAddress, WorkState),

    case EsipaReq of
	{pendingNotification, PendingNotif} ->

            % setup ES9+ request message
	    Es9Req = case PendingNotif of
			 {profileInstallationResult, PrfleInstRslt} ->
			     % SGP32-ProfileInstallationResult and ProfileInstallationResult share the
			     % exact same definition, so we may convert without an extra case statement.
			     PrfleInstRsltData = maps:get(profileInstallationResultData, PrfleInstRslt),
			     TransactionId = maps:get(transactionId, PrfleInstRsltData),
			     io:format("====TransactionId===>~p~n", [TransactionId]),
			     {handleNotification, #{pendingNotification => {profileInstallationResult, PrfleInstRslt}}};
			 {otherSignedNotification, OtherSignNotif} ->
			     % TODO: An otherSignedNotification does not contain a TransactionId. However it contains
			     % an SMDP+ OID. Maybe we can at least use this OID to lookup the BaseUrl. In any case we
			     % won't be able to lookup a specific WorkState here. (do we even need it in this case?)
			     {handleNotification, #{pendingNotification => {otherSignedNotification, OtherSignNotif}}};
			 {compactProfileInstallationResult, _CompactPrfleInstRslt} ->
			     throw("IPA Capability \"minimizeEsipaBytes\" (optional) not supported by this eIM");
			 {compactOtherSignedNotification, _CompactOtherSignNotif} ->
			     throw("IPA Capability \"minimizeEsipaBytes\" (optional) not supported by this eIM")
		     end,

            % perform ES9+ request (We expect an empty response in this case)
	    {} = es9p_client:request_json(Es9Req, BaseUrl);

	{provideEimPackageResult, _PrvdeEimPkgRslt} ->
        % TODO: This is a notification intended for the eIM itsself, we need to implement
        % some handler function and call it from here.
	    throw("TODO")
    end,

    % There is no response defined for this function (see also SGP.32, section 6.3.2.4), so we send just an empty
    % response (0 bytes of data)
    ok = mnesia_db:work_finish(maps:get(pid, Req0), success, EsipaReq),
    emptyResponse;

%GSMA SGP.32, section 6.3.2.6
handle_asn1(Req0, _State, {getEimPackageRequest, EsipaReq}) ->
    % TODO: Some state has changed on the eUICC, clarify what kind of data we should request when this happens.
    % NotifStateChg = maps:is_key(notifyStateChange, EsipaReq),

    % setup ESipa response message
    % TODO: Besides profileDownloadTriggerRequest, there is also euiccPackageRequest, ipaEuiccDataRequest, and
    % eimAcknowledgements.

    % TODO: We won't get a TransactionId here yet (except for eUICC packages where we must generate it ourselves).
    % The first time we see a TransactionId is in the SMDP+ response to the initiateAuthenticationRequest
    EidValue = maps:get(eidValue, EsipaReq),
    Work = mnesia_db:work_fetch(utils:binary_to_hex(EidValue), maps:get(pid, Req0)),
    EsipaResp = case Work of
		    {download, Order} ->
			{[{<<"activationCode">>, ActivationCode}]} = Order,
			{profileDownloadTriggerRequest, #{profileDownloadData => {activationCode, ActivationCode}}};
		    {psmo, Order} ->
                        % Convert Order to PSMO list
			Order2Psmo = fun(PsmoOrder) ->
					     case PsmoOrder of
						 {[{<<"psmo">>,<<"enable">>},{<<"iccid">>,Iccid},{<<"rollback">>, true}]} ->
						     {enable, #{iccid => utils:hex_to_binary(Iccid), rollbackFlag => null}};
						 {[{<<"psmo">>,<<"enable">>},{<<"iccid">>,Iccid},{<<"rollback">>, false}]} ->
						     {enable, #{iccid => utils:hex_to_binary(Iccid)}};
						 {[{<<"psmo">>,<<"disable">>},{<<"iccid">>,Iccid}]} ->
						     {disable, #{iccid => utils:hex_to_binary(Iccid)}};
						 {[{<<"psmo">>,<<"delete">>},{<<"iccid">>,Iccid}]} ->
						     {delete, #{iccid => utils:hex_to_binary(Iccid)}}
						 % TODO: when nothing matches, we get a crash report and nothing else
						 % happens. We should instead ensure that an undefinedError is returned
						 % to the ipad and the work is finished with a failure status.
					     end
				     end,
			PsmoList = [Order2Psmo(O) || O <- Order ],

			% Format Esipa message
			EuiccPackage = {psmoList, PsmoList},
			{ok, EimId} = application:get_env(onomondo_eim, eim_id),
			EuiccPackageSigned = #{eimId => list_to_binary(EimId),
					       eidValue => EidValue,
					       counterValue => 0, % TODO: pick a suitable value (how?)
					       transactionId => <<1,2,3,4>>, %TODO: generate a random transaction id (and store it?)
					       euiccPackage => EuiccPackage},
			{euiccPackageRequest, #{euiccPackageSigned => EuiccPackageSigned, eimSignature => <<0,0,0>>}};
		    none ->
			{eimPackageError, noEimPackageAvailable};
		    _ ->
			{eimPackageError, undefinedError}
		end,
    mnesia_db:work_update(maps:get(pid, Req0), #{}),
    {getEimPackageResponse, EsipaResp};

%GSMA SGP.32, section 6.3.2.7
handle_asn1(Req0, _State, {provideEimPackageResult, EsipaReq}) ->
    % TODO: Evaluate the contents of the EimPackageResult. This result may contain either results intended for the eIM
    % only, but it also may contain results/notifications intended to be forwarded to the SMDP+. We may forward those
    % results/notification to the SMDP+ in a similar way like we already do it in handleNotificationEsipa.
    % TODO: Depending on the contents in provideEimPackageResult we will conditionally know the TransactionId. This
    % resumbably is the case for notifications belonging to some kind of transactions. Otherwise we may have an OID
    % of the SMDP+, which we may use to determine the BaseUrl.
    ok = mnesia_db:work_finish(maps:get(pid, Req0), success, EsipaReq),
    {provideEimPackageResultResponse, undefined};

%Unsupported request
handle_asn1(_Req0, _State, _Data) ->
    %cowboy_req:reply(400, ?RESPONSE_HEADERS, <<"Unsupported Request">>, Req0).
    undefined.

% The ASN.1 encoder that is generated using erlang's (asn1ct) encodes an additional constructed tag in front of the
% actual EsipaMessageFromEimToIpa. This is probably due to the fact that EsipaMessageFromEimToIpa is a choice
% definition (anonymous) that is misinterpreted by the ASN.1 compiler. It is conspicuous that chosen tag for the excess
% constructed tag is always the tag of the chosen type. In general one would expect to see the tag of the chosen type
% followed by the length+value of the chosen type. (see also ITU-T Rec. X.690, section 8.13). In any case, since we are
% at the top level, we can work % around this by directly encoding the chosen type.
encode_eim_to_ipa({getEimPackageResponse, EimToIpaChoice}) ->
    'SGP32Definitions':encode('GetEimPackageResponse', EimToIpaChoice);
encode_eim_to_ipa({authenticateClientResponseEsipa, EimToIpaChoice}) ->
    'SGP32Definitions':encode('AuthenticateClientResponseEsipa', EimToIpaChoice);
encode_eim_to_ipa({initiateAuthenticationResponseEsipa, EimToIpaChoice}) ->
    'SGP32Definitions':encode('InitiateAuthenticationResponseEsipa', EimToIpaChoice);
encode_eim_to_ipa({getBoundProfilePackageResponseEsipa, EimToIpaChoice}) ->
    'SGP32Definitions':encode('GetBoundProfilePackageResponseEsipa', EimToIpaChoice);
encode_eim_to_ipa({cancelSessionResponseEsipa, EimToIpaChoice}) ->
    'SGP32Definitions':encode('CancelSessionResponseEsipa', EimToIpaChoice);
encode_eim_to_ipa(emptyResponse) ->
    {ok, <<>>};
encode_eim_to_ipa(EimToIpa) ->
    'SGP32Definitions':encode('EsipaMessageFromEimToIpa', EimToIpa).

% Start HTTP server
% TODO: maybe it makes sense to put this in a different module?
init(Req0, State) ->
    Req = case cowboy_req:header(<<"content-type">>, Req0) of
	      <<"application/x-gsma-rsp-asn1">> ->
		  % do the asn1 decode of the request body; dispatch to real handler
		  {ok, Data, Req1} = cowboy_req:read_body(Req0),
		  {ok, IpaToEim} = 'SGP32Definitions':decode('EsipaMessageFromIpaToEim', Data),
		  logger:notice("Rx ESipa ASN.1: ~p", [IpaToEim]),
		  EimToIpa = handle_asn1(Req1, State, IpaToEim),
		  logger:notice("Tx ESipa ASN.1: ~p", [EimToIpa]),
		  {ok, EncodedRespBody} = encode_eim_to_ipa(EimToIpa),
		  cowboy_req:reply(200, ?RESPONSE_HEADERS, EncodedRespBody, Req0);
	      _ ->
		  cowboy_req:reply(415, ?RESPONSE_HEADERS, <<"Unsupported content-type">>, Req0)
	  end,
    {ok, Req, State}.
