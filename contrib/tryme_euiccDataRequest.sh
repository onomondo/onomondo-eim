#!/bin/bash
. ./tryme.cfg
JSON='{ "eidValue" : "'$EID'", "order" : { "edr" : {"tagList" : "80BF20BF228384A5A688A9BF2B" } } }'
RC=`./restop.py -c -f edr -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh edr $RESOURCE_ID
