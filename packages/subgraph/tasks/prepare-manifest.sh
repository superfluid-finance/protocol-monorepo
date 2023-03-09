#!/usr/bin/env bash

# args
# $1 = the network

mustache="../../node_modules/mustache/bin/mustache"
NETWORK=$1

npx mustache config/$NETWORK.json subgraph.template.yaml > subgraph.yaml
npx mustache config/$NETWORK.json src/addresses.template.ts > src/addresses.ts