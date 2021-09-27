#!/usr/bin/env bash

ENVFILE=test.ignore.sh

# Exit script as soon as a command fails.
set -o errexit

# Executes cleanup function at script exit.
trap cleanup EXIT

cleanup() {
    rm -f $ENVFILE
    yarn testenv:stop
}
yarn testenv:start >/dev/null &

# wait for the test environment to boot
sleep 5

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
npx truffle exec scripts/deploy-test-environment.js | tee >(tail -n1 > $ENVFILE)
# read the TEST_RESOLVER_ADDRESS variable
source $ENVFILE

npx truffle exec scripts/print-addresses.js : >(cat)
npx truffle exec scripts/inspect-account.js : 0x00000000219ab540356cbb839cbe05303d7705fa # check vitalik
