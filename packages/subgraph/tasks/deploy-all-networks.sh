#!/bin/bash

JQ="../../node_modules/node-jq/bin/jq"

NETWORKS=( $($JQ -r .[] ./networks.json) )
[ $? == 0 ] || exit 1

for i in "${NETWORKS[@]}";do
    ./tasks/prepare-manifest.sh $i
    ./tasks/deploy-to-hosted-service-network.sh $1 $i
done
