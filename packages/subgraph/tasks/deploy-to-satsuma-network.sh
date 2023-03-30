#!/usr/bin/env bash

# args
# $1 = the version label
# $2 = the network

# From the Satsuma docs:
# https://docs.satsuma.xyz/subgraph-deploys
# cd <SUBGRAPH_DIRECTORY>

# graph deploy <SUBGRAPH_NAME> \
#   --version-label <VERSION_NAME> \
#   --node http://app.satsuma.xyz/api/subgraphs/deploy \
#   --deploy-key <DEPLOY_KEY>

graph="../../node_modules/@graphprotocol/graph-cli"

# prepare the manifest prior to deployment
# this generates the subgraph.yaml and
# inputs the correct addresses for the specified network ($2)
./tasks/prepare-manifest.sh $2

# deploy the subgraph to the satsuma endpoint ($2)
graph deploy $2 \
    --version-label $1 \
    --node https://app.satsuma.xyz/api/subgraphs/deploy \
    --deploy-key $SATSUMA_DEPLOY_KEY