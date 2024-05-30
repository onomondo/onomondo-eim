#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "order" : { "eco" : [ { "listEim" : { } } ] } }'
RC=`./restop.py -c -f eco -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
./tryme_lookup.sh eco $RESOURCE_ID
