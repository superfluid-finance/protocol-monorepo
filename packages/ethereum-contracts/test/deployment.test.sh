#!/usr/bin/env bash

ENVFILE=test.ignore.sh

# Exit script as soon as a command fails.
set -o errexit

# Executes cleanup function at script exit.
trap cleanup EXIT

cleanup() {
    rm -f $ENVFILE
    # Kill the ganache instance that we started (if we started one and if it's still running).
    if [ -n "$ganache_pid" ] && ps -p $ganache_pid > /dev/null; then
        echo "Killing ganache instance"
        kill -9 $ganache_pid
    fi
}

# randomize the ganache port
# GANACHE_PORT is understood by the truffle-config.js
export GANACHE_PORT=$(( 8555 + $RANDOM % 10000 ))

ganache_running() {
    nc -z localhost "$GANACHE_PORT"
}

start_ganache() {
    npx ganache-cli --networkId 5777 --port "$GANACHE_PORT" > /dev/null &
    ganache_pid=$!
}

if ganache_running; then
    echo "Using existing ganache instance"
else
    echo "Starting our own ganache instance"
    start_ganache
fi

#
# Test the scripts
#

# unset potential interfering environment varibles
unset TEST_RESOLVER_ADDRESS
unset NEW_TEST_RESOLVER
unset RESET_SUPERFLUID_FRAMEWORK
unset RELEASE_VERSION
unset USE_MOCKS
unset NON_UPGRADABLE
unset ENABLE_APP_WHITELISTING

# force to load artifacts from build folder instead
export DISABLE_NATIVE_TRUFFLE=1

# if any of them fail, exit
set -xe

> $ENVFILE
npx truffle --network ganache exec scripts/deploy-test-environment.js | tee >(tail -n1 > $ENVFILE)
# read the TEST_RESOLVER_ADDRESS variable
source $ENVFILE

npx truffle --network ganache exec scripts/print-addresses.js : >(cat)
npx truffle --network ganache exec scripts/inspect-account.js : 0x00000000219ab540356cbb839cbe05303d7705fa # check vitalik
