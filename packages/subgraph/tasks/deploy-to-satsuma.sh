#!/bin/bash
# $1 = the version label
# $2 = the network

# From the Satsuma docs:
# https://docs.satsuma.xyz/subgraph-deploys
# cd <SUBGRAPH_DIRECTORY>

# graph deploy <SUBGRAPH_NAME> \
#   --version-label <VERSION_NAME> \
#   --node http://app.satsuma.xyz/api/subgraphs/deploy \
#   --deploy-key <DEPLOY_KEY>

mustache="../../node_modules/mustache/bin/mustache"
graph="../../node_modules/@graphprotocol/graph-cli"

mustache config/$2.json subgraph.template.yaml > subgraph.yaml
mustache config/$2.json src/addresses.template.ts > src/addresses.ts
graph deploy $2 \
    --version-label $1 \
    --node https://app.satsuma.xyz/api/subgraphs/deploy \
    --deploy-key $SATSUMA_DEPLOY_KEY