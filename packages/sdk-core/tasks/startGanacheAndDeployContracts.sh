#!/bin/bash

set -ex
TESTENV_MNEMONIC="test test test test test test test test test test test junk"

CMD=$1

start_ganache() {
    { ganache-cli --networkId 4447 -h 0.0.0.0 --mnemonic "$TESTENV_MNEMONIC" > /dev/null& }
}

kill_ganache() {
    pkill -f "ganache-cli --networkId 4447" || true
}

if [ "$CMD" == "start" ];then
    kill_ganache
    start_ganache
    cd ../js-sdk
    chmod +x ./tasks/build-abi-js.sh
    ./tasks/build-abi-js.sh
    cd ../subgraph
    yarn deploy-contracts-local
elif [ "$CMD" == "stop" ];then
    kill_ganache
elif [ "$CMD" == "ganache-only" ];then
    kill_ganache
    start_ganache
fi