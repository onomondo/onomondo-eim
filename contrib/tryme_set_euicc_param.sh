#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "order" : { "euicc": [ { "counterValue" : 1000 }, { "consumerEuicc" : true } ] } }'
RC=`./restop.py -c -f euicc -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh euicc $RESOURCE_ID
