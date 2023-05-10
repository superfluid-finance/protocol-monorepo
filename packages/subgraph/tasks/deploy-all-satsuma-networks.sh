#!/usr/bin/env bash

# args
# $1 = the version label

# TODO: use jq and get the networks from metadata.json networks file
SATSUMA_NETWORKS=( "matic" "xdai" "eth-mainnet" "eth-sepolia" )

for i in "${SATSUMA_NETWORKS[@]}";do
    ./tasks/deploy-to-satsuma-network.sh "$1" "$i"
done
