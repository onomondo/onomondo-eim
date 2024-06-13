% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(esipa_asn1_handler_utils).

-export([handle_euiccPackageResult/3]).

handle_euiccPackageResult(Req0, EuiccPackageResult, EsipaReq) ->
    WorkBind = fun(Map) ->
		       case maps:is_key(transactionId, Map) of
			   true ->
			       mnesia_db:work_bind(maps:get(pid, Req0), maps:get(transactionId, Map));
			   _ ->
			       ok
		       end
	       end,
    
    CheckCounterValue = fun(Map) ->
				{EidValue, _, _} = mnesia_db:work_pickup(maps:get(pid, Req0)),
				CounterValueIpad = maps:get(counterValue, Map),
				{ok, CounterValueEim} = mnesia_db:euicc_counter_get(EidValue),
				case CounterValueIpad of
				    CounterValueEim ->
					ok;
				    _ ->
					logger:error("invalid euiccPackageResultSigned, counterValue mismatch: CounterValueIpad=~p, CounterValueEim=~p",
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
