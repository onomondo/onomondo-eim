% Copyright (c) 2025 Onomondo ApS & sysmocom - s.f.m.c. GmbH. All rights reserved.
%
% SPDX-License-Identifier: AGPL-3.0-only
%
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(eim_cfg).

-include_lib("public_key/include/public_key.hrl").
-export([gen_eim_configuration_data/1]).

gen_eim_configuration_data(Style) ->
    {ok, EimId} = application:get_env(onomondo_eim, eim_id),
    {ok, EsipaIp} = application:get_env(onomondo_eim, esipa_ip),
    {ok, EsipaPort} = application:get_env(onomondo_eim, esipa_port),
    {ok, EimCertPath} = application:get_env(onomondo_eim, eim_cert),
    {ok, EimCertPem} = file:read_file(EimCertPath),
    [{'Certificate', EimCertBer, not_encrypted}] = public_key:pem_decode(EimCertPem),
    {ok, EimCert} = 'PKIX1Explicit88':decode('Certificate', EimCertBer),
    EimCert_TbsCertificate = maps:get(tbsCertificate, EimCert),
    EimCert_SubjectPublicKeyInfo = maps:get(subjectPublicKeyInfo, EimCert_TbsCertificate),
    {ok, CounterValue} = application:get_env(onomondo_eim, counter_value),

    % Check certificate type
    EimCert_algorithm = maps:get(algorithm, EimCert_SubjectPublicKeyInfo),
    BrainpoolP256r1 = #{algorithm => {1,2,840,10045,2,1},
                        parameters => <<6,9,43,36,3,3,2,8,1,1,7>>},
    Prime256v1 = #{algorithm => {1,2,840,10045,2,1},
		   parameters => <<6,8,42,134,72,206,61,3,1,7>>},
    case EimCert_algorithm of
	BrainpoolP256r1 -> ok;
	Prime256v1 -> ok;
	_ ->
	    throw("Incorrect eIM certificate, only BrainpoolP256r1 or Prime256v1 may be used!")
    end,

    % Generate eIM configuration
    EimFqdn = string:join([inet:ntoa(EsipaIp), io_lib:format(":~B", [EsipaPort])], ""),
    EimConfigurationData = #{eimId => EimId, % Mandatory
			     eimFqdn => EimFqdn, % Optional, but necessary to access the eIM
			     counterValue => CounterValue, % Mandatory
			     associationToken => -1, %Optional: instruct the eUICC to calculate an association token
			     eimPublicKeyData => {eimPublicKey, EimCert_SubjectPublicKeyInfo}}, % Mandatory
    EimConfigurationDataList = [EimConfigurationData],
    AddInitialEimRequest =  #{eimConfigurationDataList => EimConfigurationDataList},

    Encoded = case Style of
		  request ->
		      % Formatted as AddInitialEimRequest
		      {ok, Asn1Encoded} = 'SGP32Definitions':encode('AddInitialEimRequest',
								    AddInitialEimRequest),
		     Asn1Encoded;
		  single ->
		      % Formatted as EimConfigurationData only
		      {ok, Asn1Encoded} = 'SGP32Definitions':encode('EimConfigurationData', EimConfigurationData),
		      Asn1Encoded
	     end,
    utils:binary_to_hex(Encoded).
