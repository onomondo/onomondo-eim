% Copyright (c) 2025 Onomondo ApS & sysmocom - s.f.m.c. GmbH. All rights reserved.
%
% SPDX-License-Identifier: AGPL-3.0-only
%
% Author: Philipp Maier <pmaier@sysmocom.de> / sysmocom - s.f.m.c. GmbH

%TODO: This code still lacks the verification of signatures, the reason for this is that it was still not possible to
%verify a signature in practice.

-module(crypto_utils).

-export([sign_euiccPackageSigned/2,
	 verify_euiccPackageResultSigned/2,
	 store_euicc_pubkey_from_authenticateResponseOk/2,
	 store_euicc_pubkey_from_ipaEuiccDataResponse/2]).

%Convert from DER encoded signature format to plain format (see also BSI TR03111 5.2.1)
der_to_plain(DERSignature) ->
    {ok, [R, S]} = 'DERSignature':decode('DERSignature', DERSignature),
    RPlain = utils:lpad_binary(utils:integer_to_bytes(R), <<0>>, 32),
    SPlain = utils:lpad_binary(utils:integer_to_bytes(S), <<0>>, 32),
    utils:join_binary_list([RPlain, SPlain]).

%Convert from plain format to DER encoded signature format (see also BSI TR03111 5.2.1)
plain_to_der(PlainSignature) ->
    R = binary_part(PlainSignature, 0, 32),
    S = binary_part(PlainSignature, 32, 32),
    DERSignature = [binary:decode_unsigned(R),binary:decode_unsigned(S)],
    {ok, DERSignatureEncoded} = 'DERSignature':encode('DERSignature', DERSignature),
    DERSignatureEncoded.

%Encode the association token as BER TLV IE
enc_association_token(AssociationToken) ->
    %TODO: replace this with a proper ASN.1 encoding function.
    AssociationTokenBinary = utils:integer_to_bytes(AssociationToken),
    AssociationTokenLength = utils:integer_to_bytes(byte_size(AssociationTokenBinary)),
    utils:join_binary_list([<<132>>, AssociationTokenLength, AssociationTokenBinary]).

sign_euiccPackageSigned(EuiccPackageSigned, EidValue) ->
    % Read the AssociationToken
    {ok, AssociationToken} = mnesia_db:euicc_param_get(EidValue, associationToken),

    %Format message to be signed
    {ok, EuiccPackageSignedEnc} = 'SGP32Definitions':encode('EuiccPackageSigned', EuiccPackageSigned),
    MsgToBeSigned = utils:join_binary_list([EuiccPackageSignedEnc, enc_association_token(AssociationToken)]),

    %Load private key from eIM certificate
    {ok, EimKeyPath} = application:get_env(onomondo_eim, eim_key),
    {ok, EimKeyPem} = file:read_file(EimKeyPath),
    [EimKeyPemEntry] = public_key:pem_decode(EimKeyPem),
    EimKeyECPrivateKey = public_key:pem_entry_decode(EimKeyPemEntry),

    %Sign message
    % We use SHA-256 as signature hash/digest, see also GSMA SGP.22, section 2.6.5 and
    % https://www.erlang.org/doc/apps/public_key/public_key#sign/4
    der_to_plain(public_key:sign(MsgToBeSigned, sha256, EimKeyECPrivateKey)).

verify_signature(Message, Signature, EidValue) ->
    DERSignature = plain_to_der(Signature),

    {ok, SubjectPublicKeyHex} = mnesia_db:euicc_param_get(EidValue, signPubKey),
    SubjectPublicKey = utils:hex_to_binary(SubjectPublicKeyHex),

    {ok, SignAlgo} = mnesia_db:euicc_param_get(EidValue, signAlgo),
    NamedCurve = case SignAlgo of
		     <<"prime256v1">> ->
			 {1,2,840,10045,3,1,7};
		     <<"brainpoolP256r1">> ->
			 {1,3,36,3,3,2,8,1,1,7};
		     _ ->
			 logger:error("invalid SignAlgo configured for eID: ~p~n", [utils:binary_to_hex(EidValue)]),
			 {}
		 end,

    ECPublicKey = {{'ECPoint',SubjectPublicKey}, {namedCurve, NamedCurve}},

    Result = public_key:verify(Message, sha256, DERSignature, ECPublicKey),
    case Result of
	true ->
	    ok;
	_ ->
	    logger:error("Signature verification failed for eID ~p, input parameters:~n" ++
			     "SubjectPublicKey=~p~n" ++
			     "NamedCurve=~p~n" ++
			     "Signature=~p~n" ++
			     "DERSignature=~p~n" ++
			     "Message=~p~n", [utils:binary_to_hex(EidValue),
					      utils:binary_to_hex(SubjectPublicKey),
					      NamedCurve,
					      utils:binary_to_hex(Signature),
					      utils:binary_to_hex(DERSignature),
					      utils:binary_to_hex(Message)]),
	    error
    end.

verify_euiccPackageResultSigned(EuiccPackageResult, EidValue) ->
    {ok, ConsumerEuicc} = mnesia_db:euicc_param_get(EidValue, consumerEuicc),
    case ConsumerEuicc of
	false ->
	    % Read the AssociationToken
	    {ok, AssociationToken} = mnesia_db:euicc_param_get(EidValue, associationToken),

	    case EuiccPackageResult of
		{euiccPackageResultSigned, EuiccPackageResultSigned} ->
		    EuiccPackageResultDataSigned = maps:get(euiccPackageResultDataSigned, EuiccPackageResultSigned),
		    EuiccSignEPR = maps:get(euiccSignEPR, EuiccPackageResultSigned),
		    {ok, EuiccPackageResultDataSigned_enc} = 'SGP32Definitions':encode('EuiccPackageResultDataSigned',
										       EuiccPackageResultDataSigned),
		    % "euiccSignEPR SHALL apply on the concatenated data objects euiccPackageResultDataSigned and
		    % eimSignature." (see also GSMA SGP.32, section 2.11.2.1)
		    MsgToBeVerfied = utils:join_binary_list([EuiccPackageResultDataSigned_enc,
							     enc_association_token(AssociationToken)]),
		    verify_signature(MsgToBeVerfied, EuiccSignEPR, EidValue);
		{euiccPackageErrorSigned, EuiccPackageErrorSigned} ->
		    EuiccPackageErrorDataSigned = maps:get(euiccPackageErrorDataSigned, EuiccPackageErrorSigned),
		    EuiccSignEPE = maps:get(euiccSignEPE, EuiccPackageErrorSigned),
		    {ok, EuiccPackageErrorDataSigned_enc} = 'SGP32Definitions':encode('EuiccPackageErrorDataSigned',
										      EuiccPackageErrorDataSigned),
		    % "euiccSignEPE SHALL apply on the concatenated data objects euiccPackageErrorDataSigned and
		    % eimSignature." (see also GSMA SGP.32, section 2.11.2.1)
		    MsgToBeVerfied = utils:join_binary_list([EuiccPackageErrorDataSigned_enc,
							     enc_association_token(AssociationToken)]),
		    verify_signature(MsgToBeVerfied, EuiccSignEPE, EidValue);
		{euiccPackageErrorUnsigned, _} ->
		    ok; % This result has no signature
		_ ->
		    error
	    end;
	_ ->
	    logger:info("omitting signature check for euiccPackageResultSigned from eID ~p (consumer eUICC)~n",
			[utils:binary_to_hex(EidValue)]),
	    ok
    end.

pubkey_from_cert(Cert) ->
    TbsCertificate = maps:get(tbsCertificate, Cert),
    SubjectPublicKeyInfo = maps:get(subjectPublicKeyInfo, TbsCertificate),
    Algorithm = maps:get(algorithm, SubjectPublicKeyInfo),
    SubjectPublicKey = maps:get(subjectPublicKey, SubjectPublicKeyInfo),
    BrainpoolP256r1 = #{algorithm => {1,2,840,10045,2,1},
                        parameters => <<6,9,43,36,3,3,2,8,1,1,7>>},
    Prime256v1 = #{algorithm => {1,2,840,10045,2,1},
		   parameters => <<6,8,42,134,72,206,61,3,1,7>>},
    NamedCurve = case Algorithm of
		     Prime256v1 ->
			 {1,2,840,10045,3,1,7};
		     BrainpoolP256r1 ->
			 {1,3,36,3,3,2,8,1,1,7};
		     _ ->
			 throw("Incorrect root CI certificate, only BrainpoolP256r1 or Prime256v1 may be used!")
		 end,
    {{'ECPoint', SubjectPublicKey}, {namedCurve, NamedCurve}}.

verify_cert(TrustedCert, VerifyCert) ->
    {ok, VerifyCertBer} = 'PKIX1Explicit88':encode('Certificate', VerifyCert),
    ECPublicKey = pubkey_from_cert(TrustedCert),
    Result = public_key:pkix_verify(VerifyCertBer, ECPublicKey),
    case Result of
	true ->
	    ok;
	_ ->
	    logger:error("Certificate verification failed,~nVerifyCert=~p,~nECPublicKey=~p~n",
			 [VerifyCert, ECPublicKey]),
	    error
    end.

get_root_cert(EumCertificate, []) ->
    logger:error("Certificate verification failed, no root certificate found,~nEumCertificate=~p~n", [EumCertificate]),
    error;
get_root_cert(EumCertificate, RootCiCertPaths) ->
    [RootCiCertPath | RootCiCertPathsTail ] = RootCiCertPaths,
    {ok, RootCiCertPem} = file:read_file(RootCiCertPath),
    [{'Certificate', RootCiCertBer, not_encrypted}] = public_key:pem_decode(RootCiCertPem),
    {ok, EumCertificateBer} = 'PKIX1Explicit88':encode('Certificate', EumCertificate),
    case public_key:pkix_is_issuer(EumCertificateBer, RootCiCertBer) of
	true ->
	    {ok, RootCiCertPem};
	_ ->
	    get_root_cert(EumCertificate, RootCiCertPathsTail)
    end.

verify_euicc_cert(EumCertificate, EuiccCertificate) ->
    {ok, RootCiCertPaths} = application:get_env(onomondo_eim, root_ci_certs),
    case get_root_cert(EumCertificate, RootCiCertPaths) of
	{ok, RootCiCertPem} ->
	    [{'Certificate', RootCiCertBer, not_encrypted}] = public_key:pem_decode(RootCiCertPem),
	    {ok, RootCiCert} = 'PKIX1Explicit88':decode('Certificate', RootCiCertBer),

	    % TODO: The certificate chain validation done here only performs a basic signature validation. However, a
	    % spec compliant certifiate chain verification should include:
	    %
	    % * expiration dates: no certificate in the chain must be expired.
	    % * CRL (certificate revocation lists, indicated in the CI cert): no revoked cert should be accepted.
	    % * serial number constraint of EUM certificate: first 8 digits of EID of eUICC certificate must be within
	    %   scope of EUM certificate.
	    % * CA certificate must
	    %   - have extension for basic constraints CA=true
	    %   - have extension for key usage "keyCertSign"
	    % * EUM certificate must
	    %   - have extension for basic constraints CA=true, pathLenConstraint == 0
	    %   - have extension for key usage "keyCertSign"
	    % * eUICC certificate must
	    %   - have extension for key usage "digitalSignature"

	    case verify_cert(RootCiCert, EumCertificate) of
		ok ->
		    case verify_cert(EumCertificate, EuiccCertificate) of
			ok ->
			    ok;
			_ ->
			    error
		    end;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.

store_euicc_pubkey(EumCertificate, EuiccCertificate, EidValue) ->
    case verify_euicc_cert(EumCertificate, EuiccCertificate) of
	ok ->
	    {{'ECPoint', SignPubKey}, {namedCurve, NamedCurve}} = pubkey_from_cert(EuiccCertificate),
	    SignAlgo = case NamedCurve of
			   {1,2,840,10045,3,1,7} ->
			       <<"prime256v1">>;
			   {1,3,36,3,3,2,8,1,1,7} ->
			       <<"brainpoolP256r1">>;
			   _ ->
			       <<"unknown">>
		       end,
	    ok = mnesia_db:euicc_param_set(EidValue, signPubKey, utils:binary_to_hex(SignPubKey)),
	    ok = mnesia_db:euicc_param_set(EidValue, signAlgo, SignAlgo),
	    ok;
	_ ->
	    error
    end.

store_euicc_pubkey_from_authenticateResponseOk(AuthRespOk, EidValue) ->
    case mnesia_db:euicc_param_get(EidValue, signPubKey) of
	{ok, <<>>} ->
	    % There is no public key stored yet for this eUICC, use the public
	    % key provided in the eUICC certificate
	    EumCertificate = maps:get(eumCertificate, AuthRespOk),
	    EuiccCertificate = maps:get(euiccCertificate, AuthRespOk),
	    store_euicc_pubkey(EumCertificate, EuiccCertificate, EidValue);
	_ ->
	    % There is already a public key stored for this eUICC
	    ok
    end.

store_euicc_pubkey_from_ipaEuiccDataResponse(IpaEuiccDataResponse, EidValue) ->
    case mnesia_db:euicc_param_get(EidValue, signPubKey) of
	{ok, <<>>} ->
	    % There is no public key stored yet for this eUICC, use the public
	    % key provided in the eUICC certificate
	    case IpaEuiccDataResponse of
		{ipaEuiccData, IpaEuiccData} ->
		    EumCertificatePresent = maps:is_key(eumCertificate, IpaEuiccData),
		    EuiccCertificatePresent = maps:is_key(euiccCertificate, IpaEuiccData),
		    if
			EumCertificatePresent and EuiccCertificatePresent ->
			    EumCertificate = maps:get(eumCertificate, IpaEuiccData),
			    EuiccCertificate = maps:get(euiccCertificate, IpaEuiccData),
			    store_euicc_pubkey(EumCertificate, EuiccCertificate, EidValue);
			true ->
			    % We need the eumCertificate and the euiccCertificate, if one of the two is missing, we can
			    % not proceed.
			    ok
		    end;
		_ ->
		    % The message format does not contain any public key information
		    ok
	    end;
	_ ->
	    % There is already a public key stored for this eUICC
	    ok
    end.
