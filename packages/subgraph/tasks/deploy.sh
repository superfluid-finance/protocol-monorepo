#!/bin/bash

# deploy.sh handles subgraph deployment and either deploys to all networks on a release (v1/dev/feature)
# if no network is specified, otherwise deploys to just that network

if [ "$1" == "" ];then
    echo "You must pass in the release configuration at a minimum."
    exit 1
fi

if [ "$2" == "" ];then
    # if no network is passed, we deploy to all networks of a release (v1/dev/feature)
    yarn deploy:$1
else
    # else we deploy to the specified network
    yarn deploy:to-$1 $2    
fi