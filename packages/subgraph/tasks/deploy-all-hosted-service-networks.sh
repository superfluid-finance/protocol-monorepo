#!/usr/bin/env bash

# args
# $1 = the configuration (v1, dev, feature)

JQ="../../node_modules/node-jq/bin/jq"

# TODO: get the networks from metadata.json networks file
HOSTED_SERVICE_NETWORKS=( $($JQ -r .[] ./networks.json) )
[ $? == 0 ] || exit 1

for i in "${HOSTED_SERVICE_NETWORKS[@]}";do
    ./tasks/deploy-to-hosted-service-network.sh "$1" "$i"
done
