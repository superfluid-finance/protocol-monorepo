#!/bin/bash

JQ="../../node_modules/node-jq/bin/jq"
mustache="../../node_modules/mustache/bin/mustache"
graph="../../node_modules/@graphprotocol/graph-cli"

CONTRACTS=( $($JQ -r .[] ./networks.json) )
[ $? == 0 ] || exit 1

chmod +x ./tasks/deploy-to-network.sh
for i in "${CONTRACTS[@]}";do
    ./tasks/deploy-to-network.sh $1 $i
done
