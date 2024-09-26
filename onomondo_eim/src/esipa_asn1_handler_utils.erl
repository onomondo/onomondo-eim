% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_asn1_handler_utils).

-export([handle_euiccPackageResult/3]).

transactionId_from_euiccPackageResult(EuiccPackageResult) ->
    case EuiccPackageResult of
	{euiccPackageResultSigned, EuiccPackageResultSigned} ->
	    EuiccPackageResultDataSigned = maps:get(euiccPackageResultDataSigned, EuiccPackageResultSigned),
	    maps:get(transactionId, EuiccPackageResultDataSigned);
	{euiccPackageErrorSigned, EuiccPackageErrorSigned} ->
	    EuiccPackageErrorDataSigned = maps:get(euiccPackageErrorDataSigned, EuiccPackageErrorSigned),
	    maps:get(transactionId, EuiccPackageErrorDataSigned);
	_ ->
	    none
	end.

process_euiccPackageResult(Req0, EuiccPackageResult, EsipaReq, TransactionId) ->
    WorkBind = fun(Map) ->
		       case maps:is_key(transactionId, Map) of
			   true ->
			       mnesia_db:work_bind(maps:get(pid, Req0), maps:get(transactionId, Map));
			   _ ->
			       ok
		       end
	       end,

    CheckCounterValue = fun(Map) ->
				{EidValue, _, _} = mnesia_db:work_pickup(maps:get(pid, Req0), TransactionId),
				CounterValueIpad = maps:get(counterValue, Map),
				{ok, CounterValueEim} = mnesia_db:euicc_param_get(EidValue, counterValue),
				case CounterValueIpad of
				    CounterValueEim ->
					ok;
				    _ ->
					logger:error("invalid euiccPackageResultSigned, counterValue mismatch: CounterValueIpad=~p, CounterValueEim=~p~n",
						     [CounterValueIpad, CounterValueEim]),
					error
				end
			end,

    Outcome = case EuiccPackageResult of
		  {euiccPackageResultSigned, EuiccPackageResultSigned} ->
		      EuiccPackageResultDataSigned = maps:get(euiccPackageResultDataSigned, EuiccPackageResultSigned),
		      WorkBind(EuiccPackageResultDataSigned),
		      case CheckCounterValue(EuiccPackageResultDataSigned) of
			  ok ->
			      esipa_rest_utils:euiccPackageResultDataSigned_to_outcome(EuiccPackageResultDataSigned);
			  _ ->
			      [{[{euiccPackageErrorCode, counterValueMismatch}]}]
		      end;
		  {euiccPackageErrorSigned, EuiccPackageErrorSigned} ->
		      EuiccPackageErrorDataSigned = maps:get(euiccPackageErrorDataSigned, EuiccPackageErrorSigned),
		      WorkBind(EuiccPackageErrorDataSigned),
		      EuiccPackageErrorCode = maps:get(euiccPackageErrorCode, EuiccPackageErrorDataSigned),
		      case CheckCounterValue(EuiccPackageErrorDataSigned) of
			  ok ->
			      [{[{euiccPackageErrorCode, EuiccPackageErrorCode}]}];
			  _ ->
			      [{[{euiccPackageErrorCode, counterValueMismatch}]}]
		      end;
		  {euiccPackageErrorUnsigned, _} ->
		      [{[{euiccPackageErrorCode, undefinedError}]}]
	      end,

    mnesia_db:work_finish(maps:get(pid, Req0), Outcome, EsipaReq).

% Handle an EuiccPackageResult, this includes everything from the handling of the work items in mnesia_db, down to
% signature checks and the generation of an appropriate outcome for the REST API.
handle_euiccPackageResult(Req0, EuiccPackageResult, EsipaReq) ->
    TransactionId = transactionId_from_euiccPackageResult(EuiccPackageResult),
    {EidValue, _, _} = mnesia_db:work_pickup(maps:get(pid, Req0), TransactionId),
    case crypto_utils:verify_euiccPackageResultSigned(EuiccPackageResult, EidValue) of
	ok ->
	    process_euiccPackageResult(Req0, EuiccPackageResult, EsipaReq, TransactionId);
	_ ->
	    mnesia_db:work_finish(maps:get(pid, Req0), [{[{procedureError, euiccSignatureInvalid}]}], EsipaReq)
    end.
