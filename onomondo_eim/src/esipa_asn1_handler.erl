% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_asn1_handler).
-behavior(cowboy_handler).

-export([init/2, terminate/3]).

-define(RESPONSE_HEADERS, #{<<"content-type">> => <<"application/x-gsma-rsp-asn1">>,
			    <<"x-admin-protocol">> => <<"gsma/rsp/v2.1.0">>}).

% Helper function to send out a list of notifications
handle_asn1_notificationList(_Req0, _State, []) ->
    ok;
handle_asn1_notificationList(Req0, State, NotificationList) ->
    [PendingNotification | NotificationListTail] = NotificationList,
    handle_asn1(Req0, State, {handleNotificationEsipa, {pendingNotification, PendingNotification}}),
    handle_asn1_notificationList(Req0, State, NotificationListTail).

%GSMA SGP.32, section 6.3.2.1
handle_asn1(Req0, _State, {initiateAuthenticationRequestEsipa, EsipaReq}) ->
    {_, _, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0), none),
    BaseUrl = maps:get(smdpAddress, EsipaReq),
    NewWorkState = WorkState#{smdpAddress => BaseUrl},
    mnesia_db:work_update(maps:get(pid, Req0), NewWorkState),

    % euiccInfo1 is also an optional field in initiateAuthenticationRequestEsipa and a mandatory field in
    % initiateAuthenticationRequest. However the field is only missing in case the IPA capability minimizeEsipaBytes is
    % used. This is an optional feature that this eIM does not support, so we can expect euiccInfo1 to be always present.

    % setup ES9+ request message
    Es9Req = {initiateAuthenticationRequest, EsipaReq},

    % perform ES9+ request
    {initiateAuthenticationResponse, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp = case Es9Resp of
		    {initiateAuthenticationOk, InitAuthOk} ->
			TransactionId = maps:get(transactionId, InitAuthOk),
			mnesia_db:work_bind(maps:get(pid, Req0), TransactionId),
			% TODO: matchingId and ctxParams1 are not defined in the ES9+ InitiateAuthenticationResponse message.
			% However in ESipa those fields are optional fields and either one of it should be populated in case an
			% AC is used (which we do). This means we should populate those fields. The matchingId can be extracted
			% from the AC, which we have in the Order. If the IPAd supports eimCtxParams1Generation then it should
			% be find if we would just add the matchingId field like so: maps:merge(InitAuthOk, #{matchingId =>
			% FIXME). Otherwise we would have to add a ctxParams1 field and populate it with the matchingId and the
			% deviceInfo. The deviceInfo can be retrieved via an eUICC data request.
			% (see GSMA SGP.32, section 3.1.2.3).
			{initiateAuthenticationOkEsipa, InitAuthOk};
		    {initiateAuthenticationError, InitAuthErr} ->
			ok = mnesia_db:work_finish(maps:get(pid, Req0),
						   [{[{procedureError, initiateAuthenticationError}]}], EsipaReq),
			{initiateAuthenticationErrorEsipa, InitAuthErr}
		end,
    {initiateAuthenticationResponseEsipa, EsipaResp};

%GSMA SGP.32, section 6.3.2.2
handle_asn1(Req0, _State, {authenticateClientRequestEsipa, EsipaReq}) ->
    TransactionId = maps:get(transactionId, EsipaReq),
    {EidValue, _, WorkState} = mnesia_db:work_pickup(maps:get(pid, Req0), TransactionId),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    AuthServResp = maps:get(authenticateServerResponse, EsipaReq),
    Es9Req = case AuthServResp of
		 {authenticateResponseOk, AuthRespOk} ->
		     % drive-by store the eUICC public key so that we can use it later to sign PSMOs or eCOs
		     crypto_utils:store_euicc_pubkey_from_authenticateResponseOk(AuthRespOk, EidValue),
		     {authenticateClientRequest,
		      #{transactionId => TransactionId,
			authenticateServerResponse => {authenticateResponseOk, AuthRespOk}}};
		 {authenticateResponseError, AuthRespErr} ->
		     ok = mnesia_db:work_finish(maps:get(pid, Req0),
						[{[{procedureError, authenticateResponseError}]}], EsipaReq),
		     {authenticateClientRequest,
		      #{transactionId => TransactionId,
			authenticateServerResponse => {authenticateResponseError, AuthRespErr}}};
		 {compactAuthenticateResponseOk, _compartAuthRespOk} ->
		     % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
		     throw("unsuppported message type \"compactAuthenticateResponseOk\"")
	     end,

    % perform ES9+ request
    {authenticateClientResponseEs9, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp = case Es9Resp of
		    {authenticateClientOk, AuthClntRespEs9} ->
			{authenticateClientOkDPEsipa, AuthClntRespEs9};
		    {authenticateClientError, AuthClntErr} ->
			ok = mnesia_db:work_finish(maps:get(pid, Req0),
						   [{[{procedureError, authenticateClientError}]}], EsipaReq),
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
		     ok = mnesia_db:work_finish(maps:get(pid, Req0),
						[{[{procedureError, downloadResponseError}]}], EsipaReq),
		     {getBoundProfilePackageRequest,
		      #{transactionId => TransactionId,
			prepareDownloadResponse => {downloadResponseError, DwnldRespErr}}};
		 {compactDownloadResponseOk, _CompactAuthRespOk} ->
		     % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
		     throw("unsuppported message type \"compactDownloadResponseOk\"")
	     end,

    % perform ES9+ request
    {getBoundProfilePackageResponse, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp = case Es9Resp of
		    {getBoundProfilePackageOk, GetBndPrflePkgOk} ->
			% (in case IPA Capability minimizeEsipaByte' is used, the transactionId has to be removed,
			%  however, this eIM does not support the IPA capability minimizeEsipaBytes)
			{getBoundProfilePackageOkEsipa, GetBndPrflePkgOk};
		    {getBoundProfilePackageError, GetBndPrflePkgErr} ->
			ok = mnesia_db:work_finish(maps:get(pid, Req0),
						   [{[{procedureError, getBoundProfilePackageError}]}], EsipaReq),
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
		     ok = mnesia_db:work_finish(maps:get(pid, Req0),
						[{[{procedureError, cancelSessionResponseError}]}], EsipaReq),
		     {cancelSessionRequestEs9,
		      #{transactionId => TransactionId,
			cancelSessionResponse => {cancelSessionResponseError, CancelSessionRespErr}}};
		 {compactCancelSessionResponseOk, _CompactCancelSessionReq} ->
		     % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
		     throw("unsuppported message type \"compactCancelSessionResponseOk\"")
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
	    case PendingNotif of
		{profileInstallationResult, PrfleInstRslt} ->
		    PrfleInstRsltData = maps:get(profileInstallationResultData, PrfleInstRslt),
		    TransactionId = maps:get(transactionId, PrfleInstRsltData),
		    NotificationMetadata = maps:get(notificationMetadata, PrfleInstRsltData),
		    BaseUrl = maps:get(notificationAddress, NotificationMetadata),
		    Es9Req = {handleNotification, #{pendingNotification => {profileInstallationResult, PrfleInstRslt}}},

		    % Under normal circumstances, the ProfileInstallationResult is sent as the last message of the
		    % Sub-procedure Profile Installation (see also GSMA SGP.22, section 3.1.3.3). The eIM uses the
		    % result data contained in this message to conclude the download and to make the download results
		    % available to the REST API user. However, in rare cases it is possible that a
		    % ProfileInstallationResult is received way too late as part of the Notification Delivery to
		    % Notification Receivers (see also GSMA SGP.32, section 3.7) procedure. By then the context in the
		    % eIM may be long gone. The eIM will be unable to match the ProfileInstallationResult to any
		    % context but it will foward it to the SMDP+ anyway.
		    case mnesia_db:work_bind(maps:get(pid, Req0), TransactionId) of
			ok ->
			    % A work item exists, foward the ProfileInstallationResult and make its contents
			    % available to the REST API user
			    case es9p_client:request_json(Es9Req, BaseUrl) of
				{} ->
				    Outcome = esipa_rest_utils:profileInstallationResult_to_outcome(PrfleInstRslt),
				    ok = mnesia_db:work_finish(maps:get(pid, Req0), Outcome, EsipaReq);
				_ ->
				    ok = mnesia_db:work_finish(maps:get(pid, Req0),
							       [{[{procedureError, handleNotificationError}]}],
							       EsipaReq)
			    end;
			_ ->
			    % No work item exists, foward the ProfileInstallationResult
			    {} = es9p_client:request_json(Es9Req, BaseUrl)
		    end;
		{otherSignedNotification, OtherSignNotif} ->
		    Es9Req = {handleNotification, #{pendingNotification => {otherSignedNotification, OtherSignNotif}}},
		    TbsOtherNotification = maps:get(tbsOtherNotification, OtherSignNotif),
		    BaseUrl = maps:get(notificationAddress, TbsOtherNotification),
		    {} = es9p_client:request_json(Es9Req, BaseUrl);
		{compactProfileInstallationResult, _CompactPrfleInstRslt} ->
		    % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
		    throw("unsuppported message type \"compactProfileInstallationResult\"");
		{compactOtherSignedNotification, _CompactOtherSignNotif} ->
		    % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
		    throw("unsuppported message type \"compactOtherSignedNotification\"")
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
    % TODO: The purpose of the notifyStateChange field in the getEimPackageRequest is to inform the eIM that some state
    % in the eUICC has changed and that the eIM (and in particular the REST API user) should perform an update of its
    % local records (eUICC data request, listProfileInfo PSMO etc...) This is a feature that this eIM does not support
    % yet. To implement the feature we could use a flag in the euicc table to tell the REST API user to perform the
    % update. Get the notifyStateChange flag like so: NotifStateChg = maps:is_key(notifyStateChange, EsipaReq).

    EidValue = maps:get(eidValue, EsipaReq),
    Work = mnesia_db:work_fetch(utils:binary_to_hex(EidValue), maps:get(pid, Req0)),
    EsipaResp = case Work of
		    {download, Order} ->
			% The first time we see a TransactionId is in the SMDP+ response to the
			% initiateAuthenticationRequest
			{[{<<"download">>, {[{<<"activationCode">>, ActivationCode}]}}]} = Order,
			mnesia_db:work_update(maps:get(pid, Req0), #{}),
			{profileDownloadTriggerRequest, #{profileDownloadData => {activationCode, ActivationCode}}};
		    {psmo, Order} ->
			TransactionIdPsmo = rand:bytes(16),
			mnesia_db:work_bind(maps:get(pid, Req0), TransactionIdPsmo),
			EuiccPackageSigned = esipa_rest_utils:psmo_order_to_euiccPackageSigned(Order, EidValue,
											       TransactionIdPsmo),
			case EuiccPackageSigned of
			    error ->
				ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, badPsmo}]}],
							   EsipaReq),
				{eimPackageError, undefinedError};
			    _ ->
				EimSignature = crypto_utils:sign_euiccPackageSigned(EuiccPackageSigned,
										    utils:binary_to_hex(EidValue)),
				mnesia_db:work_update(maps:get(pid, Req0), #{eimSignature => EimSignature}),
				{euiccPackageRequest,
				 #{euiccPackageSigned => EuiccPackageSigned,
				   eimSignature => EimSignature}}
			end;
		    {eco, Order} ->
			TransactionIdEco = rand:bytes(16),
			mnesia_db:work_bind(maps:get(pid, Req0), TransactionIdEco),
			EuiccPackageSigned = esipa_rest_utils:eco_order_to_euiccPackageSigned(Order, EidValue,
											      TransactionIdEco),
			case EuiccPackageSigned of
			    error ->
				ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, badEco}]}],
							   EsipaReq),
				{eimPackageError, undefinedError};
			    _ ->
				EimSignature = crypto_utils:sign_euiccPackageSigned(EuiccPackageSigned,
										    utils:binary_to_hex(EidValue)),
				mnesia_db:work_update(maps:get(pid, Req0), #{eimSignature => EimSignature}),
				{euiccPackageRequest,
				 #{euiccPackageSigned => EuiccPackageSigned,
				   eimSignature => EimSignature}}
			end;
		    {edr, Order} ->
			IpaEuiccDataRequest = esipa_rest_utils:edr_order_to_ipaEuiccDataRequest(Order),
			case IpaEuiccDataRequest of
			    error ->
				ok = mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, badEdr}]}],
							   EsipaReq),
				{eimPackageError, undefinedError};
			    _ ->
				IpaEuiccDataRequest
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
	    % Handle the euiccPackageResult first,
	    EuiccPackageResult = maps:get(euiccPackageResult, EPRAndNotifications),
	    ok = esipa_asn1_handler_utils:handle_euiccPackageResult(Req0, EuiccPackageResult, EsipaReq),
	    % then forward the notifications in the included notification list
	    RetrieveNotificationsListResponse = maps:get(notificationList, EPRAndNotifications),
	    case RetrieveNotificationsListResponse of
		{notificationList, NotificationList} ->
		    handle_asn1_notificationList(Req0, _State, NotificationList);
		{notificationsListResultError, NotificationsListResultError} ->
		    logger:notice("Ipad is reporting a problem to retrieve notifications,~nNotificationsListResultError=~p,~nPid=~p~n",
				  [NotificationsListResultError, maps:get(pid, Req0)]);
		UnhandledObject ->
		    % TODO: The RetrieveNotificationsListResponse may also contain other objects, in particular
		    % euiccPackageResultList and notificationAndEprList, which again includes either a
		    % notificationList or an euiccPackageResultList The spec is a bit unclear on how exactly and when
		    % those data objects shall be used, so we ignore them for now and display a notice in the log
		    logger:notice("RetrieveNotificationsListResponse with unhandled object,~UnhandledObject=~p,~nPid=~p~n",
				  [UnhandledObject, maps:get(pid, Req0)])
	    end;
	{ipaEuiccDataResponse, IpaEuiccDataResponse} ->
	    % drive-by store the eUICC public key so that we can use it later to sign PSMOs or eCOs
	    {EidValue, _, _} = mnesia_db:work_pickup(maps:get(pid, Req0), none),
	    crypto_utils:store_euicc_pubkey_from_ipaEuiccDataResponse(IpaEuiccDataResponse, EidValue),
	    Outcome = esipa_rest_utils:ipaEuiccDataResponse_to_outcome(IpaEuiccDataResponse),
	    mnesia_db:work_finish(maps:get(pid, Req0), Outcome, EsipaReq);
	{profileDownloadTriggerResult, _} ->
	    % The profileDownloadTriggerResult is sent by the IPAd in case a profile was downloaded directly from an
	    % RSP server, bypassing the eIM (see also SGP.32, section 3.2.3.1). This is a feature that this eIM does
	    % not support.
	    throw("unsuppported message type \"profileDownloadTriggerResult\"");
	{eimPackageError, EimPackageError} ->
	    Outcome = [{[{eimPackageError, EimPackageError}]}],
	    ok = mnesia_db:work_finish(maps:get(pid, Req0), Outcome, EsipaReq)
    end,
    {provideEimPackageResultResponse, undefined};

%Unsupported request
handle_asn1(Req0, _State, Request) ->
    mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, abortedOrder}]}], unsupported),
    logger:info("Handling of IPAd request failed, the request type is unsupported,~nRequest=~p,~nPid=~p~n",
		[Request, maps:get(pid, Req0)]),
    cowboy_req:reply(400, ?RESPONSE_HEADERS, <<"Unsupported Request">>, Req0).

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

% Process HTTP request
init(Req0, State) ->
    Req = case cowboy_req:header(<<"content-type">>, Req0) of
	      <<"application/x-gsma-rsp-asn1">> ->
		  % do the asn1 decode of the request body; dispatch to real handler
		  {ok, Data, Req1} = cowboy_req:read_body(Req0),
		  {ok, IpaToEim} = 'SGP32Definitions':decode('EsipaMessageFromIpaToEim', Data),
		  {EsipaMsgType, _} = IpaToEim,
		  logger:info("Handling incoming IPAd request: ~p,~nPeer=~p, Pid=~p~n",
			      [EsipaMsgType, maps:get(peer, Req0), maps:get(pid, Req0)]),
		  logger:debug("Rx ESipa ASN.1,~nPeer=~p, Pid=~p,~nIpaToEim=~p~n",
			       [maps:get(peer, Req0), maps:get(pid, Req0), IpaToEim]),
		  EimToIpa = handle_asn1(Req1, State, IpaToEim),
		  logger:debug("Tx ESipa ASN.1,~nPeer=~p,Pid=~p,~nEimToIpa=~p~n",
			       [maps:get(peer, Req0), maps:get(pid, Req0), EimToIpa]),
		  {ok, EncodedRespBody} = encode_eim_to_ipa(EimToIpa),
		  cowboy_req:reply(200, ?RESPONSE_HEADERS, EncodedRespBody, Req0);
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
	    mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, abortedOrder}]}], Reason),
	    logger:info("Handling of IPAd request terminated unexpectetly, Reason=~p Pid=~p~n",
			[Reason, maps:get(pid, Req0)]),
	    cowboy_req:reply(500, ?RESPONSE_HEADERS, <<"Internal Server Error">>, Req0)
    end.
