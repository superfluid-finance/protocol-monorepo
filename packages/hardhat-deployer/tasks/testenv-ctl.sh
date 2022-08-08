#!/bin/bash

set -xe

start_node() {
    yarn hardhat node --config test/fixture-projects/hardhat-project/hardhat.config.ts
}

stop_node() {
    pkill -f "yarn hardhat node" || true
}

if [ "$1" == "start" ]; then
    stop_node
    start_node
elif [ "$1" == "stop" ]; then
    stop_node
fi
