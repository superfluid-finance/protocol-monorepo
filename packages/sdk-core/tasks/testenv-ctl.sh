#!/usr/bin/env bash

# make sure that if any step fails, the script fails
set -xe

CMD=$1

if [ "$CMD" == "start" ];then
    ./tasks/startHardhatNode.sh "$CMD"
    ./tasks/setupTestEnvironment.sh
elif [ "$CMD" == "stop" ];then
    ./tasks/startHardhatNode.sh "$CMD"
fi
