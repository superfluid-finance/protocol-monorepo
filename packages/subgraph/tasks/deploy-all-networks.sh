#!/bin/bash

JQ="../../node_modules/node-jq/bin/jq"
mustache="../../node_modules/mustache/bin/mustache"
graph="../../node_modules/@graphprotocol/graph-cli"

NETWORKS=( $($JQ -r .[] ./networks.json) )
[ $? == 0 ] || exit 1

chmod +x ./tasks/deploy-to-network.sh
for i in "${NETWORKS[@]}";do
    ./tasks/deploy-to-network.sh $1 $i
done
