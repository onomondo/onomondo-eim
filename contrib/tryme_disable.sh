#!/bin/bash
. ./tryme.cfg

JSON='{ "eidValue" : "'$EID'", "order" : [{"psmo" : "disable", "iccid" : "'$ICCID'"}]}'
RC=`./restop.py -c -f psmo -j "$JSON"`
echo $RC

echo "---------------------------------------8<---------------------------------------"
RESOURCE_ID=`echo $RC | cut -d '/' -f 6`
echo "ResourceId =" $RESOURCE_ID
sleep 5
while true; do
    ./restop.py -l -f psmo -r $RESOURCE_ID
    sleep 5
done

