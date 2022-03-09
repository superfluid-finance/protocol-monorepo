#!/bin/env bash

# make sure that if any step fails, the script fails
set -xe

# build contracts
yarn install --frozen-lockfile
yarn build

# test contracts
yarn test

# test deployment
# TODO: to use @superfluid-finance/ethereum-contracts/tasks/testenv-ctl.sh script when available
export RELEASE_VERSION=test
export GANACHE_PORT=5600
# export GANACHE_PORT=$(( 50000 + $RANDOM % 10000 ))
echo "Ganache port: $GANACHE_PORT"
npx ganache-cli --port $GANACHE_PORT &
ganache_pid=$!
sleep 10 # wait ganache to start
DISABLE_NATIVE_TRUFFLE=1 npx truffle --network ganache exec node_modules/@superfluid-finance/ethereum-contracts/scripts/deploy-test-environment.js | tee deploy.ignore.log
export "$(tail -n1 deploy.ignore.log | awk '{ print $2 }')" # export the RESOLVER_ADDRESS, with some precaution to prevent script injection
npx truffle --network ganache exec scripts/deploy.js
kill -9 $ganache_pid
