#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build contracts & UI
yarn install --frozen-lockfile
export RELEASE_VERSION=test
export GANACHE_PORT=8545
# export GANACHE_PORT=$(( 5000 + $RANDOM % 1000 ))
echo "Ganache port: $GANACHE_PORT"
yarn ganache-cli --port $GANACHE_PORT &
ganache_pid=$!
sleep 10 # wait ganache to start
yarn deploy
yarn build

# test contracts
yarn test

kill -9 $ganache_pid




