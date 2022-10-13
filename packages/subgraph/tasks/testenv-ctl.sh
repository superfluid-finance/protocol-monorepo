#!/bin/bash

# NOTE: do export BUILT=true for subsequent test runs to reduce the amount
# of time spent rebuilding unnecessary things

# make sure that if any step fails, the script fails
set -xe

CMD=$1

if [ "$CMD" == "start" ];then
    # Deploy SF Contracts and Tokens and generate ganache.json networks file
    npx hardhat run scripts/runDeployContractsAndToken.ts --network localhost
    # Build, create and deploy subgraph locally
    yarn build-and-deploy-local
elif [ "$CMD" == "stop" ];then
    cd ../sdk-core
    ./tasks/startHardhatNode.sh $CMD
fi
