#!/bin/bash

# NOTE: do export BUILT=true for subsequent test runs to reduce the amount
# of time spent rebuilding unnecessary things

# make sure that if any step fails, the script fails
set -xe

CMD=$1

if [ "$CMD" == "start" ];then
    if [ "$BUILT" == "" ];then
        # Get ABIs and generate typechain in subgraph folder based on ABIs
        yarn getAbi
        cd ../sdk-core
        # build sdk-core because of auto linking to dependency
        yarn build
    fi
    cd ../subgraph
    # Prepare, set network, build and deploy subgraph locally
    yarn build-and-deploy-local
elif [ "$CMD" == "stop" ];then
    cd ../sdk-core
    ./tasks/startHardhatNode.sh $CMD
fi
