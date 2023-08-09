#!/usr/bin/env bash

# make sure that if any step fails, the script fails
set -xe

TESTENV_MNEMONIC="candy maple cake sugar pudding cream honey rich smooth crumble sweet treat"

CMD=$1

start_ganache() {
    ganache-cli --networkId 4447 --port 47545 --mnemonic "$TESTENV_MNEMONIC"
}

kill_ganache() {
    pkill -f "ganache-cli --networkId 4447" || true
}

if [ "$CMD" == "start" ];then
    kill_ganache
    start_ganache
elif [ "$CMD" == "stop" ];then
    kill_ganache
fi
