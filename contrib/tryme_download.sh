#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "download_order" : {"activationCode" : "'$AC'"}}'
RC=`./restop.py -c -f download -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
sleep 5
while true; do
    ./restop.py -l -f download -r $RESOURCE_ID
    sleep 5
done

