#!/bin/bash

JQ="../../node_modules/node-jq/bin/jq"
mustache="../../node_modules/mustache/bin/mustache"
graph="../../node_modules/@graphprotocol/graph-cli"

CONTRACTS=( $($JQ -r .[] ./networks.json) )
[ $? == 0 ] || exit 1

for i in "${CONTRACTS[@]}";do
    ./tasks/deploy-to-network.sh $1 $i
    # mustache config/$i.json subgraph.template.yaml > subgraph.yaml
    # mustache config/$i.json src/addresses.template.ts > src/addresses.ts
    # graph deploy
    #     --product hosted-service \
    #     superfluid-finance/protocol-$1-$i \
    #     --node https://api.thegraph.com/deploy/ \
    #     --ipfs https://api.thegraph.com/ipfs \
    #     --access-token $THEGRAPH_ACCESS_TOKEN
done
