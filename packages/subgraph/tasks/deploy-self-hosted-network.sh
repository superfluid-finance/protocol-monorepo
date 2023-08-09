#!/usr/bin/env bash

# args
# $1 = the version label
# $2 = the network
# $3 = ipfs api

GRAPH="npx --package=@graphprotocol/graph-cli -- graph"
ipfs_api=${$3:-https://ipfs-api.x.superfluid.dev}

echo "Deploying to satsuma network: $1-$2"

# prepare the manifest prior to deployment
# this generates the subgraph.yaml and
# inputs the correct addresses for the specified network ($2)
./tasks/prepare-manifest.sh "$2"

# deploy the subgraph to self-hosted endpoint
$GRAPH create protocol-v1 --node https://$2.subgraph.x.superfluid.dev

# deploy the subgraph to the satsuma endpoint ($2)
$GRAPH deploy "$2" \
    --version-label "$1" \
    --node https://$2.subgraph.x.superfluid.dev \
    --ipfs $3
