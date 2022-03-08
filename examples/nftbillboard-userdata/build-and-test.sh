#!/bin/env bash

# make sure that if any step fails, the script fails
set -xe

# build contracts & UI
yarn install --frozen-lockfile
export GANACHE_PORT=$(( 50000 + $RANDOM % 10000 ))
echo "Ganache port: $GANACHE_PORT"
yarn ganache-cli --port $GANACHE_PORT &
ganache_pid=$!
sleep 10 # wait ganache to start
yarn deploy
yarn build

kill -9 $ganache_pid

# test contracts
yarn test


