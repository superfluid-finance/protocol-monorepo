#!/bin/bash

# args
# $1 = the network

mustache="../../node_modules/mustache/bin/mustache"
mustache config/$1.json subgraph.template.yaml > subgraph.yaml
mustache config/$1.json src/addresses.template.ts > src/addresses.ts