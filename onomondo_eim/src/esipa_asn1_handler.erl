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

    {_, _, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0), none),
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
			mnesia_db:work_bind(maps:get(pid, Req0), TransactionId),
                        % TODO: InitiateAuthenticationOkEsipa has matchingId and ctxPrams1 as optional members. In case
                        % we are able to populate those fields from cached values, we should do so.
                        % maps:merge(InitAuthOk, #{matchingId => FIXME, ctxPrams1 => FIXME}),
			{initiateAuthenticationOkEsipa, InitAuthOk};
		    {initiateAuthenticationError, InitAuthErr} ->
			ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, initiateAuthenticationError}]}], EsipaReq),
			{initiateAuthenticationErrorEsipa, InitAuthErr}
		end,
    {initiateAuthenticationResponseEsipa, EsipaResp};

%GSMA SGP.32, section 6.3.2.2
handle_asn1(Req0, _State, {authenticateClientRequestEsipa, EsipaReq}) ->
    TransactionId = maps:get(transactionId, EsipaReq),
    {_, _, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0), TransactionId),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    AuthServResp = maps:get(authenticateServerResponse, EsipaReq),
    Es9Req = case AuthServResp of
		 {authenticateResponseOk, AuthRespOk} ->
		     {authenticateClientRequest,
		      #{transactionId => TransactionId,
			authenticateServerResponse => {authenticateResponseOk, AuthRespOk}}};
		 {authenticateResponseError, AuthRespErr} ->
		     ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, authenticateResponseError}]}], EsipaReq),
		     {authenticateClientRequest,
		      #{transactionId => TransactionId,
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
			ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, authenticateClientError}]}], EsipaReq),
			{authenticateClientErrorEsipa, AuthClntErr}
		end,
    {authenticateClientResponseEsipa, EsipaResp};

%GSMA SGP.32, section 6.3.2.3
handle_asn1(Req0, _State, {getBoundProfilePackageRequestEsipa, EsipaReq}) ->
    TransactionId = maps:get(transactionId, EsipaReq),
    {_, _, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0), TransactionId),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    PrepDwnldResp = maps:get(prepareDownloadResponse, EsipaReq),
    Es9Req = case PrepDwnldResp of
		 {downloadResponseOk, DwnldRespOk} ->
		     {getBoundProfilePackageRequest,
		      #{transactionId => TransactionId,
			prepareDownloadResponse => {downloadResponseOk, DwnldRespOk}}};
		 {downloadResponseError, DwnldRespErr} ->
		     ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, downloadResponseError}]}], EsipaReq),
		     {getBoundProfilePackageRequest,
		      #{transactionId => TransactionId,
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
			ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, getBoundProfilePackageError}]}], EsipaReq),
			{getBoundProfilePackageErrorEsipa, GetBndPrflePkgErr}
		end,
    {getBoundProfilePackageResponseEsipa, EsipaResp};

%GSMA SGP.32, section 6.3.2.5
handle_asn1(Req0, _State, {cancelSessionRequestEsipa, EsipaReq}) ->
    TransactionId = maps:get(transactionId, EsipaReq),
    {_, _, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0), TransactionId),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    CancelSessionResp = maps:get(cancelSessionResponse, EsipaReq),
    Es9Req = case CancelSessionResp of
		 {cancelSessionResponseOk, CancelSessionRespOk} ->
		     {cancelSessionRequestEs9,
		      #{transactionId => TransactionId,
			cancelSessionResponse => {cancelSessionResponseOk, CancelSessionRespOk}}};
		 {cancelSessionResponseError, CancelSessionRespErr} ->
		     ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, cancelSessionResponseError}]}], EsipaReq),
		     {cancelSessionRequestEs9,
		      #{transactionId => TransactionId,
			cancelSessionResponse => {cancelSessionResponseError, CancelSessionRespErr}}};
		 {compactCancelSessionResponseOk, _CompactCancelSessionReq} ->
		     throw("IPA Capability \"minimizeEsipaBytes\" (optional) not supported by this eIM")
	     end,

    % perform ES9+ request
    {cancelSessionResponseEs9, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    Outcome = esipa_rest_utils:cancelSessionResponse_to_outcome(CancelSessionResp),
    ok = mnesia_db:work_finish(maps:get(pid, Req0), Outcome, EsipaReq),

    % setup ESipa response message
    % CancelSessionResponseEsipa and CancelSessionResponseEs9 share the exact same definition, so we may convert
    % without an extra case statement.
    {cancelSessionResponseEsipa, Es9Resp};

%GSMA SGP.32, section 6.3.2.4
handle_asn1(Req0, _State, {handleNotificationEsipa, EsipaReq}) ->
    case EsipaReq of
	{pendingNotification, PendingNotif} ->
	    Rc = case PendingNotif of
		     {profileInstallationResult, PrfleInstRslt} ->
			 PrfleInstRsltData = maps:get(profileInstallationResultData, PrfleInstRslt),
			 TransactionId = maps:get(transactionId, PrfleInstRsltData),
			 {_, _, Ws} = mnesia_db:work_pickup(maps:get(pid, Req0), TransactionId),
			 Oc = esipa_rest_utils:profileInstallationResult_to_outcome(PrfleInstRslt),
			 Eq = {handleNotification,
			       #{pendingNotification => {profileInstallationResult, PrfleInstRslt}}},
			 {Ws, Oc, Eq};
		     {otherSignedNotification, OtherSignNotif} ->
			 {_, _, Ws} = mnesia_db:work_pickup(maps:get(pid, Req0), none),
			 Oc = esipa_rest_utils:otherSignedNotification_to_outcome(OtherSignNotif),
			 Eq = {handleNotification,
			       #{pendingNotification => {otherSignedNotification, OtherSignNotif}}},
			 {Ws, Oc, Eq};
		     {compactProfileInstallationResult, _CompactPrfleInstRslt} ->
		         % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
			 {_, _, Ws} = mnesia_db:work_pickup(maps:get(pid, Req0), none),
			 Oc = [{[{procedureError, handleNotificationError}]}],
			 Eq = {}, % Not supported
			 {Ws, Oc, Eq};
		     {compactOtherSignedNotification, _CompactOtherSignNotif} ->
			 % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
			 {_, _, Ws} = mnesia_db:work_pickup(maps:get(pid, Req0), none),
			 Oc = [{[{procedureError, handleNotificationError}]}],
			 Eq = {}, % Not supported
			 {Ws, Oc, Eq}
		     end,
	    {WorkState, Outcome, Es9Req} = Rc,

            % perform ES9+ request (We expect an empty response in this case)
	    BaseUrl = maps:get(smdpAddress, WorkState),
	    case es9p_client:request_json(Es9Req, BaseUrl) of
		{} ->
		    ok = mnesia_db:work_finish(maps:get(pid, Req0), Outcome, EsipaReq);
		_ ->
		    ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, handleNotificationError}]}], EsipaReq)
	    end;

	{provideEimPackageResult, _PrvdeEimPkgRslt} ->
	    %Use the already existing handle_asn1 function to prcess the provideEimPackageResult we got here
	    %(provideEimPackageResult is directed to the eIM itsself, so there will be no ES9+ request)
	    handle_asn1(Req0, _State, {provideEimPackageResult, EsipaReq})
    end,

    % There is no response defined for this function (see also SGP.32, section 6.3.2.4), so we send just an empty
    % response (0 bytes of data)
    emptyResponse;

%GSMA SGP.32, section 6.3.2.6
handle_asn1(Req0, _State, {getEimPackageRequest, EsipaReq}) ->
    % TODO: Some state has changed on the eUICC, clarify what kind of data we should request when this happens.
    % NotifStateChg = maps:is_key(notifyStateChange, EsipaReq),

    % setup ESipa response message
    % TODO: Besides profileDownloadTriggerRequest, there is also euiccPackageRequest, ipaEuiccDataRequest, and
    % eimAcknowledgements.

    % We won't get a TransactionId here yet (except for eUICC packages where we must generate it ourselves).
    % The first time we see a TransactionId is in the SMDP+ response to the initiateAuthenticationRequest
    EidValue = maps:get(eidValue, EsipaReq),
    Work = mnesia_db:work_fetch(utils:binary_to_hex(EidValue), maps:get(pid, Req0)),
    EsipaResp = case Work of
		    {download, Order} ->
			{[{<<"download">>, {[{<<"activationCode">>, ActivationCode}]}}]} = Order,
			mnesia_db:work_update(maps:get(pid, Req0), #{}),
			{profileDownloadTriggerRequest, #{profileDownloadData => {activationCode, ActivationCode}}};
		    {psmo, Order} ->
			TransactionIdPsmo = rand:bytes(16),
			mnesia_db:work_bind(maps:get(pid, Req0), TransactionIdPsmo),
			EuiccPackageSigned = esipa_rest_utils:psmo_order_to_euiccPackageSigned(Order, EidValue, TransactionIdPsmo),
			case EuiccPackageSigned of
			    error ->
				ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, badPsmo}]}], EsipaReq),
				{eimPackageError, undefinedError};
			    _ ->
				EimSignature = crypto_utils:sign_euiccPackageSigned(EuiccPackageSigned, EidValue),
				mnesia_db:work_update(maps:get(pid, Req0), #{eimSignature => EimSignature, eidValue => EidValue}),
				{euiccPackageRequest,
				 #{euiccPackageSigned => EuiccPackageSigned,
				   eimSignature => EimSignature}}
			end;
		    {eco, Order} ->
			TransactionIdEco = rand:bytes(16),
			mnesia_db:work_bind(maps:get(pid, Req0), TransactionIdEco),
			EuiccPackageSigned = esipa_rest_utils:eco_order_to_euiccPackageSigned(Order, EidValue, TransactionIdEco),
			case EuiccPackageSigned of
			    error ->
				ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, badEco}]}], EsipaReq),
				{eimPackageError, undefinedError};
			    _ ->
				EimSignature = crypto_utils:sign_euiccPackageSigned(EuiccPackageSigned, EidValue),
				mnesia_db:work_update(maps:get(pid, Req0), #{eimSignature => EimSignature, eidValue => EidValue}),
				{euiccPackageRequest,
				 #{euiccPackageSigned => EuiccPackageSigned,
				   eimSignature => EimSignature}}
			end;
		    none ->
			{eimPackageError, noEimPackageAvailable};
		    _ ->
			ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, badOrder}]}], EsipaReq),
			{eimPackageError, undefinedError}
		end,
    {getEimPackageResponse, EsipaResp};

%GSMA SGP.32, section 6.3.2.7
handle_asn1(Req0, _State, {provideEimPackageResult, EsipaReq}) ->
    case EsipaReq of
	{euiccPackageResult, EuiccPackageResult} ->
	    ok = esipa_asn1_handler_utils:handle_euiccPackageResult(Req0, EuiccPackageResult, EsipaReq);
	{ePRAndNotifications, EPRAndNotifications} ->
	    % TODO: Do something useful with the notificationList, that is also included in this response
	    EuiccPackageResult = maps:get(euiccPackageResult, EPRAndNotifications),
	    ok = esipa_asn1_handler_utils:handle_euiccPackageResult(Req0, EuiccPackageResult, EsipaReq);
	{ipaEuiccDataResponse, _} ->
	    throw("TODO: Implement handling of incoming EuiccDataResponse");
	{profileDownloadTriggerResult, _} ->
	    throw("TODO: Implement handling of incoming profileDownloadTriggerResult");
	{eimPackageError, EimPackageError} ->
	    Outcome = [{[{eimPackageError, EimPackageError}]}],
	    ok = mnesia_db:work_finish(maps:get(pid, Req0), Outcome, EsipaReq)
    end,
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
