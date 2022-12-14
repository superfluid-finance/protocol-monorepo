#!/bin/bash

# args
# $1 = the configuration (v1, dev, feature)
# $2 = the network

if [ "$1" == "" ];then
    echo "You must pass in the release configuration at a minimum."
    exit 1
fi

if [ "$2" == "" ];then
    # if no network is passed, we deploy to all networks of a release (v1/dev/feature)
    ./tasks/deploy-all-networks.sh $1
else
    # else we deploy to the specified network ($1 = release, $2 = network)
    ./tasks/deploy-to-hosted-service-network.sh $1 $2
fi