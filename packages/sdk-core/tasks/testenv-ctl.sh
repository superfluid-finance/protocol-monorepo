#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

CMD=$1

if [ "$CMD" == "start" ];then
    ./tasks/startGanacheAndDeployContracts.sh $CMD
    # Come back to sdk-core after going to subgraph folder
    cd ../sdk-core
    yarn generate-graphql-types
    yarn generate-abi-files
    yarn generate-web3-types
elif [ "$CMD" == "stop" ];then
    ./tasks/startGanacheAndDeployContracts.sh $CMD
fi