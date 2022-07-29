#!/bin/bash

# NOTE: do export BUILT=true for subsequent test runs to reduce the amount
# of time spent rebuilding unnecessary things

# make sure that if any step fails, the script fails
set -xe

CMD=$1

if [ "$CMD" == "start" ];then
    if [ "$BUILT" == "" ];then
        cd ../ethereum-contracts
        # Install contract dependencies and build contracts
        pnpm install --frozen-lockfile
        yarn run build:contracts
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
    # Install subgraph dependencies
    if [ "$BUILT" == "" ];then
        pnpm install --frozen-lockfile
    fi
    # Deploy contracts and token locally
    yarn deploy-contracts-local
    # Prepare, set network, build and deploy subgraph locally
    yarn build-and-deploy-local
elif [ "$CMD" == "stop" ];then
    cd ../sdk-core
    ./tasks/startHardhatNode.sh $CMD
fi
