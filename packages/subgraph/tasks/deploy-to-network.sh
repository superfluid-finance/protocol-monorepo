#!/bin/bash
# $1 = the configuration (v1, dev, feature)
# $2 = the network

mustache="../../node_modules/mustache/bin/mustache"
graph="../../node_modules/@graphprotocol/graph-cli"

mustache config/$2.json subgraph.template.yaml > subgraph.yaml
mustache config/$2.json src/addresses.template.ts > src/addresses.ts
graph deploy \
    --product hosted-service \
    superfluid-finance/protocol-$1-$2 \
    --node https://api.thegraph.com/deploy/ \
    --ipfs https://api.thegraph.com/ipfs \
    --access-token $THE_GRAPH_ACCESS_TOKEN
