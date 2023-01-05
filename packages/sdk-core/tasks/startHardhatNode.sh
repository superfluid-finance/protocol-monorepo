#!/usr/bin/env bash

set -ex

CMD=$1

start_hardhat_node() {
    yarn start-node --hostname 0.0.0.0 > /dev/null&
}

kill_hardhat_node() {
    pkill -f "start-node"
}

if [ "$CMD" == "start" ];then
    start_hardhat_node
elif [ "$CMD" == "stop" ];then
    kill_hardhat_node
fi