#!/usr/bin/env bash

# args
# $1 = the configuration (v1, dev, feature)

JQ="npx --package=node-jq -- jq"

# TODO: get the networks from metadata.json networks file
# shellcheck disable=SC2207
HOSTED_SERVICE_NETWORKS=( $($JQ -r .[] ./hosted-service-networks.json) ) || exit 1

for i in "${HOSTED_SERVICE_NETWORKS[@]}";do
    ./tasks/deploy-to-hosted-service-network.sh "$1" "$i"
done
