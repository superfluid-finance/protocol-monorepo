#!/bin/bash

cd "$(dirname "$0")"
set -xe

# Apply a workaround due to https://github.com/crytic/echidna/issues/738
mkdir -p node_modules
ln -sf ../../../node_modules/\@superfluid-finance node_modules/
ln -sf ../../../node_modules/\@openzeppelin node_modules/

function test_contract() {
    TEST_CONTRACT=$1
    echidna-test . --config <(cat echidna.yaml contracts/$TEST_CONTRACT.yaml) --contract $TEST_CONTRACT
}

echo "Starting a hot fuzz on contract: $1"
npx hardhat compile --force && test_contract $1
echo "Fizzles away."
