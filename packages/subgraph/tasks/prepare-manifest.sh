#!/bin/bash

# args
# $1 = the network

mustache="../../node_modules/mustache/bin/mustache"
npx mustache config/$1.json subgraph.template.yaml > subgraph.yaml
npx mustache config/$1.json src/addresses.template.ts > src/addresses.ts