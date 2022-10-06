#!/bin/bash

# NOTE: do export BUILT=true for subsequent test runs to reduce the amount
# of time spent rebuilding unnecessary things

# make sure that if any step fails, the script fails
set -xe

CMD=$1

if [ "$CMD" == "start" ];then
    if [ "$BUILT" == "" ];then
        # NOTE: this is already done outside, but it gives time for the subgraph to
        # start syncing
        cd ../ethereum-contracts
        yarn build:contracts-truffle
        yarn build:contracts-hardhat
        # Get ABIs and generate typechain in subgraph folder based on ABIs
        cd ../subgraph
        yarn getAbi
        yarn run generate-ethers-types
        cd ../js-sdk
        # Get abi.js file for js-sdk to deploy locally
        chmod +x ./tasks/build-abi-js.sh
        ./tasks/build-abi-js.sh
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
