#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "order" : { "euicc": [ { "counterValue" : 1000 }, { "consumerEuicc" : false }, { "signAlgo" : "prime256v1" }, { "signPubKey" : "04BD2C55B28B4E801CA0B14F195788FD86C7FF49764C88A2934C69594A26FF647549F3F16060BD9C8D793D95D0126FA429C94966FE7842967263795A73C498F1DB" } ] } }'
RC=`./restop.py -c -f euicc -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh euicc $RESOURCE_ID
