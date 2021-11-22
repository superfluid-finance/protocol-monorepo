#!/bin/bash

JQ="../../node_modules/node-jq/bin/jq"
mustache="../../node_modules/mustache/bin/mustache"
graph="../../node_modules/@graphprotocol/graph-cli"

CONTRACTS=( $($JQ -r .[] ./networks.json) )
[ $? == 0 ] || exit 1

for i in "${CONTRACTS[@]}";do
    mustache config/$i.json subgraph.template.yaml > subgraph.yaml
    mustache config/$i.json src/addresses.template.ts > src/addresses.ts
    SUBGRAPH_NAME=superfluid-finance/protocol-$1-$i
    yarn deploy
done