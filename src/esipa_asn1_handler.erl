% Author: Harald Welte <hwelte@sysmocom.de> / sysmocom - s.f.m.c. GmbH
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_asn1_handler).

-export([handle_asn1/2]).

% Helper function to send out a list of notifications
handle_asn1_notificationList(_Pid, []) ->
    ok;
handle_asn1_notificationList(Pid, NotificationList) ->
    [PendingNotification | NotificationListTail] = NotificationList,
    handle_asn1(Pid, {handleNotificationEsipa, {pendingNotification, PendingNotification}}),
    handle_asn1_notificationList(Pid, NotificationListTail).

%GSMA SGP.32, section 6.3.2.1
handle_asn1(Pid, {initiateAuthenticationRequestEsipa, EsipaReq}) ->
    {_, _, WorkState} = mnesia_db:work_pickup(Pid, none),
    BaseUrl = maps:get(smdpAddress, EsipaReq),
    NewWorkState = WorkState#{smdpAddress => BaseUrl},
    mnesia_db:work_update(Pid, NewWorkState),

    % euiccInfo1 is also an optional field in initiateAuthenticationRequestEsipa and a mandatory field in
    % initiateAuthenticationRequest. However the field is only missing in case the IPA capability minimizeEsipaBytes is
    % used. This is an optional feature that this eIM does not support, so we can expect euiccInfo1 to be always present.

    % setup ES9+ request message
    Es9Req = {initiateAuthenticationRequest, EsipaReq},

    % perform ES9+ request
    {initiateAuthenticationResponse, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp =
        case Es9Resp of
            {initiateAuthenticationOk, InitAuthOk} ->
                TransactionId = maps:get(transactionId, InitAuthOk),
                mnesia_db:work_bind(Pid, TransactionId),
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
                ok = mnesia_db:work_finish(
                    Pid,
                    [{[{procedureError, initiateAuthenticationError}]}],
                    EsipaReq
                ),
                {initiateAuthenticationErrorEsipa, InitAuthErr}
        end,
    {initiateAuthenticationResponseEsipa, EsipaResp};
%GSMA SGP.32, section 6.3.2.2
handle_asn1(Pid, {authenticateClientRequestEsipa, EsipaReq}) ->
    TransactionId = maps:get(transactionId, EsipaReq),
    {EidValue, _, WorkState} = mnesia_db:work_pickup(Pid, TransactionId),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    AuthServResp = maps:get(authenticateServerResponse, EsipaReq),
    Es9Req =
        case AuthServResp of
            {authenticateResponseOk, AuthRespOk} ->
                % drive-by store the eUICC public key so that we can use it later to sign PSMOs or eCOs
                crypto_utils:store_euicc_pubkey_from_authenticateResponseOk(AuthRespOk, EidValue),
                {authenticateClientRequest, #{
                    transactionId => TransactionId,
                    authenticateServerResponse => {authenticateResponseOk, AuthRespOk}
                }};
            {authenticateResponseError, AuthRespErr} ->
                ok = mnesia_db:work_finish(
                    Pid,
                    [{[{procedureError, authenticateResponseError}]}],
                    EsipaReq
                ),
                {authenticateClientRequest, #{
                    transactionId => TransactionId,
                    authenticateServerResponse => {authenticateResponseError, AuthRespErr}
                }};
            {compactAuthenticateResponseOk, _compartAuthRespOk} ->
                % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
                throw("unsuppported message type \"compactAuthenticateResponseOk\"")
        end,

    % perform ES9+ request
    {authenticateClientResponseEs9, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp =
        case Es9Resp of
            {authenticateClientOk, AuthClntRespEs9} ->
                {authenticateClientOkDPEsipa, AuthClntRespEs9};
            {authenticateClientError, AuthClntErr} ->
                ok = mnesia_db:work_finish(
                    Pid,
                    [{[{procedureError, authenticateClientError}]}],
                    EsipaReq
                ),
                {authenticateClientErrorEsipa, AuthClntErr}
        end,
    {authenticateClientResponseEsipa, EsipaResp};
%GSMA SGP.32, section 6.3.2.3
handle_asn1(Pid, {getBoundProfilePackageRequestEsipa, EsipaReq}) ->
    TransactionId = maps:get(transactionId, EsipaReq),
    {_, _, WorkState} = mnesia_db:work_pickup(Pid, TransactionId),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    PrepDwnldResp = maps:get(prepareDownloadResponse, EsipaReq),
    Es9Req =
        case PrepDwnldResp of
            {downloadResponseOk, DwnldRespOk} ->
                {getBoundProfilePackageRequest, #{
                    transactionId => TransactionId,
                    prepareDownloadResponse => {downloadResponseOk, DwnldRespOk}
                }};
            {downloadResponseError, DwnldRespErr} ->
                ok = mnesia_db:work_finish(
                    Pid,
                    [{[{procedureError, downloadResponseError}]}],
                    EsipaReq
                ),
                {getBoundProfilePackageRequest, #{
                    transactionId => TransactionId,
                    prepareDownloadResponse => {downloadResponseError, DwnldRespErr}
                }};
            {compactDownloadResponseOk, _CompactAuthRespOk} ->
                % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
                throw("unsuppported message type \"compactDownloadResponseOk\"")
        end,

    % perform ES9+ request
    {getBoundProfilePackageResponse, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    % setup ESipa response message
    EsipaResp =
        case Es9Resp of
            {getBoundProfilePackageOk, GetBndPrflePkgOk} ->
                % (in case IPA Capability minimizeEsipaByte' is used, the transactionId has to be removed,
                %  however, this eIM does not support the IPA capability minimizeEsipaBytes)
                {getBoundProfilePackageOkEsipa, GetBndPrflePkgOk};
            {getBoundProfilePackageError, GetBndPrflePkgErr} ->
                ok = mnesia_db:work_finish(
                    Pid,
                    [{[{procedureError, getBoundProfilePackageError}]}],
                    EsipaReq
                ),
                {getBoundProfilePackageErrorEsipa, GetBndPrflePkgErr}
        end,
    {getBoundProfilePackageResponseEsipa, EsipaResp};
%GSMA SGP.32, section 6.3.2.5
handle_asn1(Pid, {cancelSessionRequestEsipa, EsipaReq}) ->
    TransactionId = maps:get(transactionId, EsipaReq),
    {_, _, WorkState} = mnesia_db:work_pickup(Pid, TransactionId),
    BaseUrl = maps:get(smdpAddress, WorkState),

    % setup ES9+ request message
    CancelSessionResp = maps:get(cancelSessionResponse, EsipaReq),
    Es9Req =
        case CancelSessionResp of
            {cancelSessionResponseOk, CancelSessionRespOk} ->
                {cancelSessionRequestEs9, #{
                    transactionId => TransactionId,
                    cancelSessionResponse => {cancelSessionResponseOk, CancelSessionRespOk}
                }};
            {cancelSessionResponseError, CancelSessionRespErr} ->
                ok = mnesia_db:work_finish(
                    Pid,
                    [{[{procedureError, cancelSessionResponseError}]}],
                    EsipaReq
                ),
                {cancelSessionRequestEs9, #{
                    transactionId => TransactionId,
                    cancelSessionResponse => {cancelSessionResponseError, CancelSessionRespErr}
                }};
            {compactCancelSessionResponseOk, _CompactCancelSessionReq} ->
                % IPA Capability "minimizeEsipaBytes" (optional) is not supported by this eIM
                throw("unsuppported message type \"compactCancelSessionResponseOk\"")
        end,

    % perform ES9+ request
    {cancelSessionResponseEs9, Es9Resp} = es9p_client:request_json(Es9Req, BaseUrl),

    Outcome = esipa_rest_utils:cancelSessionResponse_to_outcome(CancelSessionResp),
    ok = mnesia_db:work_finish(Pid, Outcome, EsipaReq),

    % setup ESipa response message
    % CancelSessionResponseEsipa and CancelSessionResponseEs9 share the exact same definition, so we may convert
    % without an extra case statement.
    {cancelSessionResponseEsipa, Es9Resp};
%GSMA SGP.32, section 6.3.2.4
handle_asn1(Pid, {handleNotificationEsipa, EsipaReq}) ->
    case EsipaReq of
        {pendingNotification, PendingNotif} ->
            case PendingNotif of
                {profileInstallationResult, PrfleInstRslt} ->
                    PrfleInstRsltData = maps:get(profileInstallationResultData, PrfleInstRslt),
                    TransactionId = maps:get(transactionId, PrfleInstRsltData),
                    NotificationMetadata = maps:get(notificationMetadata, PrfleInstRsltData),
                    BaseUrl = maps:get(notificationAddress, NotificationMetadata),
                    Es9Req =
                        {handleNotification, #{
                            pendingNotification => {profileInstallationResult, PrfleInstRslt}
                        }},

                    % Under normal circumstances, the ProfileInstallationResult is sent as the last message of the
                    % Sub-procedure Profile Installation (see also GSMA SGP.22, section 3.1.3.3). The eIM uses the
                    % result data contained in this message to conclude the download and to make the download results
                    % available to the REST API user. However, in rare cases it is possible that a
                    % ProfileInstallationResult is received way too late as part of the Notification Delivery to
                    % Notification Receivers (see also GSMA SGP.32, section 3.7) procedure. By then the context in the
                    % eIM may be long gone. The eIM will be unable to match the ProfileInstallationResult to any
                    % context but it will foward it to the SMDP+ anyway.
                    case mnesia_db:work_bind(Pid, TransactionId) of
                        ok ->
                            % A work item exists, foward the ProfileInstallationResult and make its contents
                            % available to the REST API user
                            case es9p_client:request_json(Es9Req, BaseUrl) of
                                {} ->
                                    Outcome = esipa_rest_utils:profileInstallationResult_to_outcome(
                                        PrfleInstRslt
                                    ),
                                    ok = mnesia_db:work_finish(Pid, Outcome, EsipaReq);
                                _ ->
                                    ok = mnesia_db:work_finish(
                                        Pid,
                                        [{[{procedureError, handleNotificationError}]}],
                                        EsipaReq
                                    )
                            end;
                        _ ->
                            % No work item exists, foward the ProfileInstallationResult
                            {} = es9p_client:request_json(Es9Req, BaseUrl)
                    end;
                {otherSignedNotification, OtherSignNotif} ->
                    Es9Req =
                        {handleNotification, #{
                            pendingNotification => {otherSignedNotification, OtherSignNotif}
                        }},
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
            handle_asn1(Pid, {provideEimPackageResult, EsipaReq})
    end,

    % There is no response defined for this function (see also SGP.32, section 6.3.2.4), so we send just an empty
    % response (0 bytes of data)
    emptyResponse;
%GSMA SGP.32, section 6.3.2.6
handle_asn1(Pid, {getEimPackageRequest, EsipaReq}) ->
    % TODO: The purpose of the notifyStateChange field in the getEimPackageRequest is to inform the eIM that some state
    % in the eUICC has changed and that the eIM (and in particular the REST API user) should perform an update of its
    % local records (eUICC data request, listProfileInfo PSMO etc...) This is a feature that this eIM does not support
    % yet. To implement the feature we could use a flag in the euicc table to tell the REST API user to perform the
    % update. Get the notifyStateChange flag like so: NotifStateChg = maps:is_key(notifyStateChange, EsipaReq).

    EidValue = maps:get(eidValue, EsipaReq),
    Work = mnesia_db:work_fetch(utils:binary_to_hex(EidValue), Pid),
    EsipaResp =
        case Work of
            {download, Order} ->
                % The first time we see a TransactionId is in the SMDP+ response to the
                % initiateAuthenticationRequest
                {[{<<"download">>, {[{<<"activationCode">>, ActivationCode}]}}]} = Order,
                mnesia_db:work_update(Pid, #{}),
                {profileDownloadTriggerRequest, #{
                    profileDownloadData => {activationCode, ActivationCode}
                }};
            {psmo, Order} ->
                TransactionIdPsmo = rand:bytes(16),
                mnesia_db:work_bind(Pid, TransactionIdPsmo),
                EuiccPackageSigned = esipa_rest_utils:psmo_order_to_euiccPackageSigned(
                    Order,
                    EidValue,
                    TransactionIdPsmo
                ),
                case EuiccPackageSigned of
                    error ->
                        ok = mnesia_db:work_finish(
                            Pid,
                            [{[{procedureError, badPsmo}]}],
                            EsipaReq
                        ),
                        {eimPackageError, undefinedError};
                    _ ->
                        EimSignature = crypto_utils:sign_euiccPackageSigned(
                            EuiccPackageSigned,
                            utils:binary_to_hex(EidValue)
                        ),
                        {euiccPackageRequest, #{
                            euiccPackageSigned => EuiccPackageSigned,
                            eimSignature => EimSignature
                        }}
                end;
            {eco, Order} ->
                TransactionIdEco = rand:bytes(16),
                mnesia_db:work_bind(Pid, TransactionIdEco),
                EuiccPackageSigned = esipa_rest_utils:eco_order_to_euiccPackageSigned(
                    Order,
                    EidValue,
                    TransactionIdEco
                ),
                case EuiccPackageSigned of
                    error ->
                        ok = mnesia_db:work_finish(
                            Pid,
                            [{[{procedureError, badEco}]}],
                            EsipaReq
                        ),
                        {eimPackageError, undefinedError};
                    _ ->
                        EimSignature = crypto_utils:sign_euiccPackageSigned(
                            EuiccPackageSigned,
                            utils:binary_to_hex(EidValue)
                        ),
                        {euiccPackageRequest, #{
                            euiccPackageSigned => EuiccPackageSigned,
                            eimSignature => EimSignature
                        }}
                end;
            {edr, Order} ->
                IpaEuiccDataRequest = esipa_rest_utils:edr_order_to_ipaEuiccDataRequest(Order),
                case IpaEuiccDataRequest of
                    error ->
                        ok = mnesia_db:work_finish(
                            Pid,
                            [{[{procedureError, badEdr}]}],
                            EsipaReq
                        ),
                        {eimPackageError, undefinedError};
                    _ ->
                        IpaEuiccDataRequest
                end;
            none ->
                {eimPackageError, noEimPackageAvailable};
            _ ->
                ok = mnesia_db:work_finish(Pid, [{[{procedureError, badOrder}]}], EsipaReq),
                {eimPackageError, undefinedError}
        end,
    {getEimPackageResponse, EsipaResp};
%GSMA SGP.32, section 6.3.2.7
handle_asn1(Pid, {provideEimPackageResult, EsipaReq}) ->
    case EsipaReq of
        {euiccPackageResult, EuiccPackageResult} ->
            ok = esipa_asn1_handler_utils:handle_euiccPackageResult(
                Pid, EuiccPackageResult, EsipaReq
            );
        {ePRAndNotifications, EPRAndNotifications} ->
            % Handle the euiccPackageResult first,
            EuiccPackageResult = maps:get(euiccPackageResult, EPRAndNotifications),
            ok = esipa_asn1_handler_utils:handle_euiccPackageResult(
                Pid, EuiccPackageResult, EsipaReq
            ),
            % then forward the notifications in the included notification list
            RetrieveNotificationsListResponse = maps:get(notificationList, EPRAndNotifications),
            case RetrieveNotificationsListResponse of
                {notificationList, NotificationList} ->
                    handle_asn1_notificationList(Pid, NotificationList);
                {notificationsListResultError, NotificationsListResultError} ->
                    logger:notice(
                        "Ipad is reporting a problem to retrieve notifications,~nNotificationsListResultError=~p,~nPid=~p~n",
                        [NotificationsListResultError, Pid]
                    );
                UnhandledObject ->
                    % TODO: The RetrieveNotificationsListResponse may also contain other objects, in particular
                    % euiccPackageResultList and notificationAndEprList, which again includes either a
                    % notificationList or an euiccPackageResultList The spec is a bit unclear on how exactly and when
                    % those data objects shall be used, so we ignore them for now and display a notice in the log
                    logger:notice(
                        "RetrieveNotificationsListResponse with unhandled object,~UnhandledObject=~p,~nPid=~p~n",
                        [UnhandledObject, Pid]
                    )
            end;
        {ipaEuiccDataResponse, IpaEuiccDataResponse} ->
            % drive-by store the eUICC public key so that we can use it later to sign PSMOs or eCOs
            {EidValue, _, _} = mnesia_db:work_pickup(Pid, none),
            crypto_utils:store_euicc_pubkey_from_ipaEuiccDataResponse(
                IpaEuiccDataResponse, EidValue
            ),
            Outcome = esipa_rest_utils:ipaEuiccDataResponse_to_outcome(IpaEuiccDataResponse),
            mnesia_db:work_finish(Pid, Outcome, EsipaReq);
        {profileDownloadTriggerResult, _} ->
            % The profileDownloadTriggerResult is sent by the IPAd in case a profile was downloaded directly from an
            % RSP server, bypassing the eIM (see also SGP.32, section 3.2.3.1). This is a feature that this eIM does
            % not support.
            throw("unsuppported message type \"profileDownloadTriggerResult\"");
        {eimPackageError, EimPackageError} ->
            Outcome = [{[{eimPackageError, EimPackageError}]}],
            ok = mnesia_db:work_finish(Pid, Outcome, EsipaReq)
    end,
    {provideEimPackageResultResponse, undefined};
%Unsupported request
handle_asn1(Pid, Request) ->
    mnesia_db:work_finish(Pid, [{[{procedureError, abortedOrder}]}], unsupported),
    logger:info(
        "Handling of IPAd request failed, the request type is unsupported,~nRequest=~p,~nPid=~p~n",
        [Request, Pid]
    ),
    {error, unsupported_request}.
