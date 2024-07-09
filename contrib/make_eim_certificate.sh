#!/bin/bash

# This script is to illustrate how an eIM certificate could be generated using
# openssl. See also: GSMA SGP.32, section 2.6.4.2

#prime256v1: X9.62/SECG curve over a 256 bit prime field
openssl ecparam -genkey -name prime256v1 -noout -out sample_eim_cert_NIST.key
openssl req -key sample_eim_cert_NIST.key -new -x509 -days 36500 -out sample_eim_cert_NIST.crt

#brainpoolP256r1: RFC 5639 curve over a 256 bit prime field
openssl ecparam -genkey -name brainpoolP256r1 -noout -out sample_eim_cert_brainpool.key
openssl req -key sample_eim_cert_brainpool.key -new -x509 -days 36500 -out sample_eim_cert_brainpool.crt

