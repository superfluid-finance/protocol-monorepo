#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

CMD=$1

if [ "$CMD" == "start" ];then
    # Generate Typechain files and move to subgraph directory
    cd ../ethereum-contracts
    # Install contract dependencies and build contracts
    yarn install --frozen-lockfile
    if [ "$BUILT" == "" ];then
        yarn run build:contracts
    fi
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
    cd ../subgraph
    # Install subgraph dependencies
    yarn install --frozen-lockfile
    # Deploy contracts and token locally
    yarn deploy-contracts-local
    # Prepare, set network, build and deploy subgraph locally
    yarn build-and-deploy-local
elif [ "$CMD" == "stop" ];then
    cd ../sdk-core
    ./tasks/startHardhatNode.sh $CMD
fi
