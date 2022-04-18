#!/bin/bash

cd "$(dirname "$0")"

set -xe

# workaround for https://github.com/crytic/echidna/issues/738
mkdir -p node_modules
ln -sf ../../../node_modules/\@superfluid-finance node_modules/
ln -sf ../../../node_modules/\@openzeppelin node_modules/

function testsuite() {
    TEST_CONTRACT=$1
    echidna-test . --config <(cat echidna.yaml contracts/$TEST_CONTRACT.yaml) --contract $TEST_CONTRACT
}

echo "Fuzzing $1"

npx truffle compile --all && testsuite $1
