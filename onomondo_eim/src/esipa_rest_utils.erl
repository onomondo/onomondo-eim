% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_rest_utils).

-export([order_to_euiccPackageSigned/2,
	 euiccPackageResultDataSigned_to_outcome/1,
	 profileInstallationResult_to_outcome/1,
	 otherSignedNotification_to_outcome/1,
	 cancelSessionResponse_to_outcome/1]).

psmo_to_asn_enable(Psmo) ->
    case Psmo of
	{[{<<"iccid">>, Iccid}, {<<"rollback">>, true}]} ->
	    {enable, #{iccid => utils:hex_to_binary(Iccid), rollbackFlag => null}};
	{[{<<"iccid">>, Iccid}, {<<"rollback">>, false}]} ->
	    {enable, #{iccid => utils:hex_to_binary(Iccid)}};
	_ ->
	    error
    end.

psmo_to_asn_disable(Psmo) ->
    case Psmo of
	{[{<<"iccid">>,Iccid}]} ->
	    {disable, #{iccid => utils:hex_to_binary(Iccid)}};
	_ ->
	    error
    end.

psmo_to_asn_delete(Psmo) ->
    case Psmo of
	{[{<<"iccid">>,Iccid}]} ->
	    {delete, #{iccid => utils:hex_to_binary(Iccid)}};
	_ ->
	    error
    end.

psmo_to_asn_listProfileInfo(Psmo) ->
    case Psmo of
	{[{<<"searchCriteria">>, SearchCriteria}, {<<"tagList">>, TagList}]} ->
	    case SearchCriteria of
		{[{<<"isdpAid">>, IsdpAid}]} ->
		    {listProfileInfo, #{searchCriteria => {isdpAid, utils:hex_to_binary(IsdpAid)}, tagList => utils:hex_to_binary(TagList) }};
		{[{<<"iccid">>, Iccid}]} ->
		    {listProfileInfo, #{searchCriteria => {iccid, utils:hex_to_binary(Iccid)}, tagList => utils:hex_to_binary(TagList) }};
		{[{<<"profileClass">>, ProfileClass}]} ->
		    {listProfileInfo, #{searchCriteria => {profileClass, utils:hex_to_integer(ProfileClass)}, tagList => utils:hex_to_binary(TagList) }};
		_ ->
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
		    error
	    end;
	{[{<<"tagList">>, TagList}]} ->
	    {listProfileInfo, #{tagList => utils:hex_to_binary(TagList) }};
	{[]} ->
	    {listProfileInfo, #{}};
	_ ->
	    error
    end.

psmo_to_asn_getRAT(Psmo) ->
    case Psmo of
	{[]} ->
	    {getRAT, #{}};
	_ ->
	    error
    end.

psmo_to_asn_configureAutoEnable(Psmo) ->
    case Psmo of
	{[{<<"autoEnableFlag">>, true}, {<<"smdpOid">>, SmdpOid}, {<<"smdpAddress">>, SmdpAddress}]} ->
	    {configureAutoEnable, #{autoEnableFlag => null, smdpOid => binary_to_list(SmdpOid), smdpAddress => SmdpAddress}};
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
	    error
    end.

eco_to_asn_addEim(Eco) ->
    case Eco of
	{[{<<"eimConfigurationData">>, EimCfgEnc}]} ->
	    {ok, EimCfg} = 'SGP32Definitions':decode('EimConfigurationData', utils:hex_to_binary(EimCfgEnc)),
	    {addEim, EimCfg};
	_ ->
	    error
    end.

eco_to_asn_deleteEim(Eco) ->
    case Eco of
	{[{<<"eimId">>, EimId}]} ->
	    {deleteEim, #{eimId => EimId}};
	_ ->
	    error
    end.

eco_to_asn_updateEim(Eco) ->
    case Eco of
	{[{<<"eimConfigurationData">>, EimCfgEnc}]} ->
	    {ok, EimCfg} = 'SGP32Definitions':decode('EimConfigurationData', utils:hex_to_binary(EimCfgEnc)),
	    {updateEim, EimCfg};
	_ ->
	    error
    end.

eco_to_asn_listEim(Psmo) ->
    case Psmo of
	{[]} ->
	    {listEim, #{}};
	_ ->
	    error
    end.

% Generate an euiccPackageSigned from an Order (JSON REST API)
order_to_euiccPackageSigned(Order, EidValue) ->
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
				error
			end
		end,

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
				error
			end
		end,

    % Convert Order to PSMO list
    EuiccPackage = case Order of
		       % TODO: add support for eCO
		       {[{<<"psmo">>, PsmoOrderList}]} ->
			   PsmoList = [Order2Psmo(O) || O <- PsmoOrderList ],
			   case lists:member(error, PsmoList) of
			       true ->
				   % At least one PSMO has failed the conversion from JSON to ASN.
				   error;
			       false ->
				   {psmoList, PsmoList}
			   end;
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

    % Format EuiccPackageSigned
    case EuiccPackage of
	error ->
	    % The EuiccPackage was not generated properly
	    error;
	_ ->
	    {ok, EimId} = application:get_env(onomondo_eim, eim_id),
	    #{eimId => list_to_binary(EimId),
	      eidValue => EidValue,
	      counterValue => 0, % TODO: pick a suitable value (how?)
	      transactionId => <<1,2,3,4>>, %TODO: generate a random transaction id (and store it?)
	      euiccPackage => EuiccPackage}
    end.

% generate a JSON encodeable outcome (JSON REST API) from an EuiccPackageResultDataSigned
euiccPackageResultDataSigned_to_outcome(EuiccPackageResultDataSigned) ->
    EuiccResult = maps:get(euiccResult, EuiccPackageResultDataSigned),
    Error = {[{<<"error">>, <<"malformedResult">>}]},
    EuiccResultData2Json = fun(EuiccResultData) ->
				   case EuiccResultData of
				       {enableResult, EnableResult} ->
					   {[{<<"enableResult">>, EnableResult}]};
				       {disableResult, DisableResult} ->
					   {[{<<"disableResult">>, DisableResult}]};
				       {deleteResult, DeleteResult} ->
					   {[{<<"deleteResult">>, DeleteResult}]};
				       {listProfileInfoResult, _} ->
					   % TODO: extract useful information from listProfileInfoResult
					   % and format it as JSON encodeable outcome
					   {[{<<"listProfileInfoResult">>, <<"absent">>}]};
				       {getRATResult, _} ->
					   % TODO: extract useful information from getRATResult
					   % and format it as JSON encodeable outcome
					   {[{<<"getRATResult">>, <<"absent">>}]};
				       {configureAutoEnableResult, ConfigureAutoEnableResult} ->
					   {[{<<"configureAutoEnableResult">>, ConfigureAutoEnableResult}]};
				       {addEimResult, _} ->
					   % TODO: extract useful information from addEimResult
					   % and format it as JSON encodeable outcome
					   {[{<<"addEimResult">>, <<"absent">>}]};
				       {deleteEimResult, DeleteEimResult} ->
					   {[{<<"deleteEimResult">>, DeleteEimResult}]};
				       {updateEimResult, UpdateEimResult} ->
					   {[{<<"updateEimResult">>, UpdateEimResult}]};
				       {listEimResult, _} ->
					   % TODO: extract useful information from listEimResult
					   % and format it as JSON encodeable outcome
					   {[{<<"listEimResult">>, <<"absent">>}]};
				       {rollbackResult, RollbackResult} ->
					   {[{<<"rollbackResult">>, RollbackResult}]};
				       {processingTerminated, ProcessingTerminated} ->
					   {[{<<"processingTerminated">>, ProcessingTerminated}]};
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
	    [{[{<<"profileInstallationResult">>,
		{[{ <<"finalResult">>, <<"successResult">>},
		  { <<"iccid">>, utils:binary_to_hex(Iccid)}]}
	       }]}];
	_ ->
	    [{[{<<"profileInstallationResult">>,
		{[{ <<"finalResult">>, <<"errorResult">>}]}
	       }]}]
    end.

% generate a JSON encodeable outcome (JSON REST API) from an OtherSignedNotification
otherSignedNotification_to_outcome(_OtherSignedNotification) ->
    % TODO: extract some useful information from OtherSignedNotification
    [{[{<<"notificationResult">>, <<"otherSignedNotification">>}]}].

% generate a JSON encodeable outcome (JSON REST API) from an CancelSessionResponse
cancelSessionResponse_to_outcome(CancelSessionResponse) ->
    case CancelSessionResponse of
	{cancelSessionResponseOk, _} ->
	    [{[{<<"cancelSessionResult">>, ok}]}];
	_ ->
	    [{[{<<"cancelSessionResult">>, error}]}]
    end.
