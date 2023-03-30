#!/usr/bin/env bash
# $1 = the configuration (v1, dev, feature)
# $2 = the network

graph="../../node_modules/@graphprotocol/graph-cli"

# prepare the manifest prior to deployment
# this generates the subgraph.yaml and
# inputs the correct addresses for the specified network ($2)
./tasks/prepare-manifest.sh $2

# deploy the subgraph to the hosted service (protocol-$1-$2)
graph deploy \
    --product hosted-service \
    superfluid-finance/protocol-$1-$2 \
    --node https://api.thegraph.com/deploy/ \
    --ipfs https://api.thegraph.com/ipfs \
    --access-token $THE_GRAPH_ACCESS_TOKEN
