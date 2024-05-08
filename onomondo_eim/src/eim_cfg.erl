% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

-module(eim_cfg).

-include_lib("public_key/include/public_key.hrl").
-export([gen_eim_configuration_data/0]).

gen_eim_configuration_data() ->
    {ok, EimId} = application:get_env(onomondo_eim, eim_id),
    {ok, EsipaIp} = application:get_env(onomondo_eim, esipa_ip),
    {ok, EsipaPort} = application:get_env(onomondo_eim, esipa_port),
    {ok, EsipaSslCertPath} = application:get_env(onomondo_eim, esipa_ssl_cert),
    {ok, EsipaSslCertPem} = file:read_file(EsipaSslCertPath),
    [{'Certificate', EsipaSslCertBer, not_encrypted}] = public_key:pem_decode(EsipaSslCertPem),
    {ok, EsipaSslCert} = 'PKIX1Explicit88':decode('Certificate', EsipaSslCertBer),
    EsipaSslCert_TbsCertificate = maps:get(tbsCertificate, EsipaSslCert),
    EsipaSslCert_SubjectPublicKeyInfo = maps:get(subjectPublicKeyInfo, EsipaSslCert_TbsCertificate),
    EimFqdn = string:join([inet:ntoa(EsipaIp), io_lib:format(":~B", [EsipaPort])], ""),
    EimConfigurationData = #{eimId => EimId, % Mandatory
			     eimFqdn => EimFqdn, % Optional, but necessary to access the eIM
			     counterValue => 0, % Mandatory
			     associationToken => -1, %Optional: instruct the eUICC to calculate an association token
			     eimPublicKeyData => {eimPublicKey, EsipaSslCert_SubjectPublicKeyInfo}}, % Mandatory
    EimConfigurationDataList = [EimConfigurationData],
    GetEimConfigurationDataResponse = #{eimConfigurationDataList => EimConfigurationDataList},

    {ok, EncodedGetEimConfigurationDataResponse} = 'SGP32Definitions':encode('GetEimConfigurationDataResponse', GetEimConfigurationDataResponse),
    utils:binary_to_hex(EncodedGetEimConfigurationDataResponse).
