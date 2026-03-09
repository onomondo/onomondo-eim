% Copyright (c) 2025 Onomondo ApS & sysmocom - s.f.m.c. GmbH. All rights reserved.
%
% SPDX-License-Identifier: AGPL-3.0-only
%
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_rest_utils).

-export([psmo_order_to_euiccPackageSigned/3,
	 eco_order_to_euiccPackageSigned/3,
	 euiccPackageResultDataSigned_to_outcome/1,
	 profileInstallationResult_to_outcome/1,
	 cancelSessionResponse_to_outcome/1,
	 edr_order_to_ipaEuiccDataRequest/1,
	 ipaEuiccDataResponse_to_outcome/1]).

psmo_to_asn_enable(Psmo) ->
    case Psmo of
	{[{<<"iccid">>, Iccid}, {<<"rollback">>, true}]} ->
	    {enable, #{iccid => utils:hex_to_binary(Iccid), rollbackFlag => null}};
	{[{<<"iccid">>, Iccid}, {<<"rollback">>, false}]} ->
	    {enable, #{iccid => utils:hex_to_binary(Iccid)}};
	_ ->
	    logger:error("REST: order with bad enable PSMO: ~p~n", [Psmo]),
	    error
    end.

psmo_to_asn_disable(Psmo) ->
    case Psmo of
	{[{<<"iccid">>,Iccid}]} ->
	    {disable, #{iccid => utils:hex_to_binary(Iccid)}};
	_ ->
	    logger:error("REST: order with bad disable PSMO: ~p~n", [Psmo]),
	    error
    end.

psmo_to_asn_delete(Psmo) ->
    case Psmo of
	{[{<<"iccid">>,Iccid}]} ->
	    {delete, #{iccid => utils:hex_to_binary(Iccid)}};
	_ ->
	    logger:error("REST: order with bad delete PSMO: ~p~n", [Psmo]),
	    error
    end.

psmo_to_asn_listProfileInfo(Psmo) ->
    case Psmo of
	{[{<<"searchCriteria">>, SearchCriteria}, {<<"tagList">>, TagList}]} ->
	    case SearchCriteria of
		{[{<<"isdpAid">>, IsdpAid}]} ->
		    {listProfileInfo, #{searchCriteria => {isdpAid, utils:hex_to_binary(IsdpAid)},
					tagList => utils:hex_to_binary(TagList) }};
		{[{<<"iccid">>, Iccid}]} ->
		    {listProfileInfo, #{searchCriteria => {iccid, utils:hex_to_binary(Iccid)},
					tagList => utils:hex_to_binary(TagList) }};
		{[{<<"profileClass">>, ProfileClass}]} ->
		    {listProfileInfo, #{searchCriteria => {profileClass, utils:hex_to_integer(ProfileClass)},
					tagList => utils:hex_to_binary(TagList) }};
		_ ->
		    logger:error("REST: order with bad listProfilesInfo PSMO: ~p~n", [Psmo]),
		    error
	    end;
	{[{<<"searchCriteria">>, SearchCriteria}]} ->
	    case SearchCriteria of
		{[{<<"isdpAid">>, IsdpAid}]} ->
		    {listProfileInfo, #{searchCriteria => {isdpAid, utils:hex_to_binary(IsdpAid)} }};
		{[{<<"iccid">>, Iccid}]} ->
		    {listProfileInfo, #{searchCriteria => {iccid, utils:hex_to_binary(Iccid)} }};
		{[{<<"profileClass">>, ProfileClass}]} ->
		    {listProfileInfo, #{searchCriteria => {profileClass, utils:hex_to_integer(ProfileClass)} }};
		_ ->
		    logger:error("REST: order with bad listProfilesInfo PSMO: ~p~n", [Psmo]),
		    error
	    end;
	{[{<<"tagList">>, TagList}]} ->
	    {listProfileInfo, #{tagList => utils:hex_to_binary(TagList) }};
	{[]} ->
	    {listProfileInfo, #{}};
	_ ->
	    logger:error("REST: order with bad listProfilesInfo PSMO: ~p~n", [Psmo]),
	    error
    end.

psmo_to_asn_getRAT(Psmo) ->
    case Psmo of
	{[]} ->
	    {getRAT, #{}};
	_ ->
	    logger:error("REST: order with bad getRAT PSMO: ~p~n", [Psmo]),
	    error
    end.

psmo_to_asn_configureAutoEnable(Psmo) ->
    case Psmo of
	{[{<<"autoEnableFlag">>, true}, {<<"smdpOid">>, SmdpOid}, {<<"smdpAddress">>, SmdpAddress}]} ->
	    {configureAutoEnable, #{autoEnableFlag => null, smdpOid => binary_to_list(SmdpOid),
				    smdpAddress => SmdpAddress}};
	{[{<<"autoEnableFlag">>, true}, {<<"smdpOid">>, SmdpOid}]} ->
	    {configureAutoEnable, #{autoEnableFlag => null, smdpOid => binary_to_list(SmdpOid)}};
	{[{<<"autoEnableFlag">>, true}, {<<"smdpAddress">>, SmdpAddress}]} ->
	    {configureAutoEnable, #{autoEnableFlag => null, smdpAddress => SmdpAddress}};
	{[{<<"autoEnableFlag">>, true}]} ->
	    {configureAutoEnable, #{autoEnableFlag => null}};
	{[{<<"autoEnableFlag">>, false}, {<<"smdpOid">>, SmdpOid}, {<<"smdpAddress">>, SmdpAddress}]} ->
	    {configureAutoEnable, #{mdpOid => binary_to_list(SmdpOid), smdpAddress => SmdpAddress}};
	{[{<<"autoEnableFlag">>, false}, {<<"smdpOid">>, SmdpOid}]} ->
	    {configureAutoEnable, #{smdpOid => binary_to_list(SmdpOid)}};
	{[{<<"autoEnableFlag">>, false}, {<<"smdpAddress">>, SmdpAddress}]} ->
	    {configureAutoEnable, #{smdpAddress => SmdpAddress}};
	{[{<<"autoEnableFlag">>, false}]} ->
	    {configureAutoEnable, #{}};
	_ ->
	    logger:error("REST: order with bad configureAutoEnable PSMO: ~p~n", [Psmo]),
	    error
    end.

format_euicc_EuiccPackageSigned(EuiccPackage, EidValue, TransactionId) ->
    case EuiccPackage of
	error ->
	    % The EuiccPackage was not generated properly
	    error;
	_ ->
	    {ok, EimId} = application:get_env(onomondo_eim, eim_id),
	    {ok, CounterValue} = mnesia_db:euicc_counter_tick(utils:binary_to_hex(EidValue)),
	    #{eimId => list_to_binary(EimId),
	      eidValue => EidValue,
	      counterValue => CounterValue,
	      transactionId => TransactionId,
	      euiccPackage => EuiccPackage}
    end.

% Generate an euiccPackageSigned from a PSMO Order (JSON REST API)
psmo_order_to_euiccPackageSigned(Order, EidValue, TransactionId) ->
   Order2Psmo = fun(PsmoOrder) ->
			case PsmoOrder of
			    {[{<<"enable">>, Psmo}]} ->
				psmo_to_asn_enable(Psmo);
			    {[{<<"disable">>, Psmo}]} ->
				psmo_to_asn_disable(Psmo);
			    {[{<<"delete">>, Psmo}]} ->
				psmo_to_asn_delete(Psmo);
			    {[{<<"listProfileInfo">>, Psmo}]} ->
				psmo_to_asn_listProfileInfo(Psmo);
			    {[{<<"getRAT">>, Psmo}]} ->
				psmo_to_asn_getRAT(Psmo);
			    {[{<<"configureAutoEnable">>, Psmo}]} ->
				psmo_to_asn_configureAutoEnable(Psmo);
			    _ ->
				logger:error("REST: order with unknown/unsupported PSMO: ~p~n", [PsmoOrder]),
				error
			end
		end,

    % Convert Order to PSMO list
    EuiccPackage = case Order of
		       {[{<<"psmo">>, PsmoOrderList}]} ->
			   PsmoList = [Order2Psmo(O) || O <- PsmoOrderList],
			   case lists:member(error, PsmoList) of
			       true ->
				   % At least one PSMO has failed the conversion from JSON to ASN.
				   error;
			       false ->
				   {psmoList, PsmoList}
			   end;
		       _ ->
			   error
		   end,

    format_euicc_EuiccPackageSigned(EuiccPackage, EidValue, TransactionId).

eco_to_asn_addEim(Eco) ->
    case Eco of
	{[{<<"eimConfigurationData">>, EimCfgEnc}]} ->
	    {ok, EimCfg} = 'SGP32Definitions':decode('EimConfigurationData', utils:hex_to_binary(EimCfgEnc)),
	    {addEim, EimCfg};
	_ ->
	    logger:error("REST: order with bad addEim eCO: ~p~n", [Eco]),
	    error
    end.

eco_to_asn_deleteEim(Eco) ->
    case Eco of
	{[{<<"eimId">>, EimId}]} ->
	    {deleteEim, #{eimId => EimId}};
	_ ->
	    logger:error("REST: order with bad deleteEim eCO: ~p~n", [Eco]),
	    error
    end.

eco_to_asn_updateEim(Eco) ->
    case Eco of
	{[{<<"eimConfigurationData">>, EimCfgEnc}]} ->
	    {ok, EimCfg} = 'SGP32Definitions':decode('EimConfigurationData', utils:hex_to_binary(EimCfgEnc)),
	    {updateEim, EimCfg};
	_ ->
	    logger:error("REST: order with bad updateEim eCO: ~p~n", [Eco]),
	    error
    end.

eco_to_asn_listEim(Eco) ->
    case Eco of
	{[]} ->
	    {listEim, #{}};
	_ ->
	    logger:error("REST: order with bad listEim eCO: ~p~n", [Eco]),
	    error
    end.

% Generate an euiccPackageSigned from a eCO Order (JSON REST API)
eco_order_to_euiccPackageSigned(Order, EidValue, TransactionId) ->
    Order2Eco = fun(EcoOrder) ->
			case EcoOrder of
			    {[{<<"addEim">>, Eco }]} ->
				eco_to_asn_addEim(Eco);
			    {[{<<"deleteEim">>, Eco }]} ->
				eco_to_asn_deleteEim(Eco);
			    {[{<<"updateEim">>, Eco }]} ->
				eco_to_asn_updateEim(Eco);
			    {[{<<"listEim">>, Eco }]} ->
				eco_to_asn_listEim(Eco);
			    _ ->
				logger:error("REST: order with unknown/unsupported eCO: ~p~n", [EcoOrder]),
				error
			end
		end,

    % Convert Order to PSMO list
    EuiccPackage = case Order of
		       {[{<<"eco">>, EcoOrderList}]} ->
			   EcoList = [Order2Eco(O) || O <- EcoOrderList ],
			   case lists:member(error, EcoList) of
			       true ->
				   % At least one eCO has failed the conversion from JSON to ASN.
				   error;
			       false ->
				   {ecoList, EcoList}
			   end;
		       _ ->
			   error
		   end,

    format_euicc_EuiccPackageSigned(EuiccPackage, EidValue, TransactionId).

memberOrNil(Key, Map) ->
    case maps:is_key(Key, Map) of
	true ->
	    {Key, maps:get(Key, Map)};
	_ ->
	    nil
    end.

memberOrNilHex(Key, Map) ->
    case maps:is_key(Key, Map) of
	true ->
	    {Key, utils:binary_to_hex(maps:get(Key, Map))};
	_ ->
	    nil
    end.

memberOrNilAsnHex(Key, Map, AsnSpec, AsnType) ->
    case maps:is_key(Key, Map) of
	true ->
	    Member = maps:get(Key, Map),
	    {ok, MemberAsn1Encoded} = AsnSpec:encode(AsnType, Member),
	    {Key, utils:binary_to_hex(MemberAsn1Encoded)};
	_ ->
	    nil
    end.





result_to_json_listProfileInfoResult(ListProfileInfoResult) ->
    ProfileInfo2Json = fun(ProfileInfo) ->
			       List = [
				       memberOrNilHex(iccid, ProfileInfo),
				       memberOrNilHex(isdpAid, ProfileInfo),
				       memberOrNil(profileState, ProfileInfo),
				       memberOrNil(profileNickname, ProfileInfo),
				       memberOrNil(serviceProviderName, ProfileInfo),
				       memberOrNil(profileName, ProfileInfo),
				       memberOrNil(iconType, ProfileInfo),
				       memberOrNilHex(icon, ProfileInfo),
				       memberOrNil(profileClass, ProfileInfo)
				       % TODO: also extract useful information from the following (optional) fields:
				       % notificationConfigurationInfo
				       % profileOwner
				       % dpProprietaryData
				       % profilePolicyRules
				       % serviceSpecificDataStoredInEuicc
				      ],
			       {lists:filter(fun(Member) -> Member /= nil end, List)}
		       end,

    case ListProfileInfoResult of
	{profileInfoListOk, ProfileInfoListOk} ->
	    ProfileInfoList = [ProfileInfo2Json(O) || O <- ProfileInfoListOk],
	    ListProfileInfoResultValue = {[{finalResult, successResult}, {profileInfoList, ProfileInfoList}]},
	    {[{listProfileInfoResult, ListProfileInfoResultValue}]};
        _ ->
	    {[{listProfileInfoResult, {[{finalResult, errorResult}]}}]}
    end.

result_to_json_listEimResult(ListEimResult) ->
    EimIdList2Json = fun(EimIdInfo) ->
			       List = [
				       memberOrNil(eimId, EimIdInfo),
				       memberOrNil(eimIdType, EimIdInfo)
				      ],
			       {lists:filter(fun(Member) -> Member /= nil end, List)}
		       end,

    case ListEimResult of
	{eimIdList, EimIdList} ->
	    EimIdInfoList = [EimIdList2Json(O) || O <- EimIdList],
	    ListEimResultValue = {[{finalResult, successResult}, {eimIdList, EimIdInfoList}]},
	    {[{listEimResult, ListEimResultValue}]};
        _ ->
	    {[{listEimResult, {[{finalResult, errorResult}]}}]}
    end.

result_to_json_addEimResult(AddEimResult) ->
    case AddEimResult of
	{associationToken, AssociationToken} ->
	    {[{addEimResult, {[{addEimResultCode, ok}, {associationToken, AssociationToken}]}}]};
	{addEimResultCode, AddEimResultCode} ->
	    {[{addEimResult, {[{addEimResultCode, AddEimResultCode}]}}]};
	_ ->
	    {[{addEimResult, {[{addEimResultCode, malformedResult}]}}]}
    end.

result_to_json_getRATResult(GetRATResult) ->
    Op2Json = fun(Op) ->
		      List = [{mccMnc, utils:binary_to_hex(maps:get(mccMnc,Op))},
			       memberOrNilHex(gid1, Op),
			       memberOrNilHex(gid2, Op)
			     ],
		      {lists:filter(fun(Member) -> Member /= nil end, List)}
	      end,
    Ppr2Json = fun(Ppr) ->
		       {[{pprUpdateControl, lists:member(pprUpdateControl, maps:get(pprIds, Ppr))},
			  {ppr1, lists:member(ppr1, maps:get(pprIds, Ppr))},
			  {ppr2, lists:member(ppr2, maps:get(pprIds, Ppr))},
			  {allowedOperators, [Op2Json(O) || O <- maps:get(allowedOperators, Ppr)]},
			  {consentRequired, lists:member(consentRequired, maps:get(pprFlags, Ppr))}]}
			end,
    RulesAuthorisationTable = [Ppr2Json(O) || O <- GetRATResult],
    {[{getRATResult, RulesAuthorisationTable}]}.

% generate a JSON encodeable outcome (JSON REST API) from an EuiccPackageResultDataSigned
euiccPackageResultDataSigned_to_outcome(EuiccPackageResultDataSigned) ->
    EuiccResult = maps:get(euiccResult, EuiccPackageResultDataSigned),
    Error = {[{error, malformedResult}]},
    EuiccResultData2Json = fun(EuiccResultData) ->
				   case EuiccResultData of
				       {enableResult, EnableResult} ->
					   {[{enableResult, EnableResult}]};
				       {disableResult, DisableResult} ->
					   {[{disableResult, DisableResult}]};
				       {deleteResult, DeleteResult} ->
					   {[{deleteResult, DeleteResult}]};
				       {listProfileInfoResult, ListProfileInfoResult} ->
					   result_to_json_listProfileInfoResult(ListProfileInfoResult);
				       {getRATResult, GetRATResult} ->
					   result_to_json_getRATResult(GetRATResult);
				       {configureAutoEnableResult, ConfigureAutoEnableResult} ->
					   {[{configureAutoEnableResult, ConfigureAutoEnableResult}]};
				       {addEimResult, AddEimResult} ->
					   result_to_json_addEimResult(AddEimResult);
				       {deleteEimResult, DeleteEimResult} ->
					   {[{deleteEimResult, DeleteEimResult}]};
				       {updateEimResult, UpdateEimResult} ->
					   {[{updateEimResult, UpdateEimResult}]};
				       {listEimResult, ListEimResult} ->
					   result_to_json_listEimResult(ListEimResult);
				       {rollbackResult, RollbackResult} ->
					   {[{rollbackResult, RollbackResult}]};
				       {processingTerminated, ProcessingTerminated} ->
					   {[{processingTerminated, ProcessingTerminated}]};
				       _ ->
					   Error
				   end
			   end,
    [EuiccResultData2Json(O) || O <- EuiccResult ].

% generate a JSON encodeable outcome (JSON REST API) from an ProfileInstallationResult
profileInstallationResult_to_outcome(ProfileInstallationResult) ->
    ProfileInstallationResultData = maps:get(profileInstallationResultData, ProfileInstallationResult),
    NotificationMetadata = maps:get(notificationMetadata, ProfileInstallationResultData),
    FinalResult = maps:get(finalResult, ProfileInstallationResultData),

    case FinalResult of
	{successResult, _} ->
	    Iccid = maps:get(iccid, NotificationMetadata),
	    [{[{profileInstallationResult,
		{[{finalResult, successResult},
		  {iccid, utils:binary_to_hex(Iccid)}]}
	       }]}];
	_ ->
	    [{[{profileInstallationResult,
		{[{finalResult, errorResult}]}
	       }]}]
    end.

% generate a JSON encodeable outcome (JSON REST API) from an CancelSessionResponse
cancelSessionResponse_to_outcome(CancelSessionResponse) ->
    case CancelSessionResponse of
	{cancelSessionResponseOk, _} ->
	    [{[{cancelSessionResult, ok}]}];
	_ ->
	    [{[{cancelSessionResult, undefinedError}]}]
    end.

% Generate an ipaEuiccDataRequest from a eDR Order (JSON REST API)
edr_order_to_ipaEuiccDataRequest(Order) ->
    % TODO: also add support for the two optional parameters SubjectKeyIdentifier and searchCriteria
    case Order of
	{[{<<"edr">>,{[{<<"tagList">>, TagList}]}}]} ->
	    {ipaEuiccDataRequest, #{tagList => utils:hex_to_binary(TagList)}};
	_ ->
	    error
    end.

% generate a JSON encodeable outcome (JSON REST API) from an ipaEuiccDataResponse
ipaEuiccDataResponse_to_outcome(IpaEuiccDataResponse) ->
    case IpaEuiccDataResponse of
	{ipaEuiccData, IpaEuiccData} ->
	    IpaEuiccDataJson = [
				memberOrNil(defaultSmdpAddress, IpaEuiccData),
				memberOrNilAsnHex(euiccInfo1, IpaEuiccData,
						  'RSPDefinitions', 'EUICCInfo1'),
				memberOrNilAsnHex(euiccInfo2, IpaEuiccData,
						  'SGP32Definitions', 'SGP32-EUICCInfo2'),
				memberOrNil(rootSmdsAddress, IpaEuiccData),
				memberOrNil(associationToken, IpaEuiccData),
				memberOrNilAsnHex(eumCertificate, IpaEuiccData,
						  'PKIX1Explicit88', 'Certificate'),
				memberOrNilAsnHex(euiccCertificate, IpaEuiccData,
						  'PKIX1Explicit88', 'Certificate'),
				memberOrNilAsnHex(ipaCapabilities, IpaEuiccData,
						  'SGP32Definitions', 'IpaCapabilities'),
				memberOrNilAsnHex(deviceInfo, IpaEuiccData,
						  'RSPDefinitions', 'DeviceInfo'),
				memberOrNilAsnHex(notificationsList, IpaEuiccData,
						  'SGP32Definitions', 'SGP32-RetrieveNotificationsListResponse')
			       ],
	    IpaEuiccDataJsonFiltered = {lists:filter(fun(Member) -> Member /= nil end, IpaEuiccDataJson)},
	    [{[{euiccDataResult, {[{edrResult, ok}, {euiccData, IpaEuiccDataJsonFiltered}]}}]}];
	{ipaEuiccDataError, IpaEuiccDataError} ->
	    [{[{euiccDataResult, {[{edrResult, IpaEuiccDataError}]}}]}];
	_ ->
	    [{[{euiccDataResult, {[{edrResult, undefinedError}]}}]}]
    end.
