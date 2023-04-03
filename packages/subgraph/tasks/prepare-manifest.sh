#!/usr/bin/env bash

# args
# $1 = the network

mustache="npx mustache"

$mustache config/"$1".json subgraph.template.yaml > subgraph.yaml
$mustache config/"$1".json src/addresses.template.ts > src/addresses.ts
