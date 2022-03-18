#!/bin/bash

function testsuite() {
    TEST_CONTRACT=$1
    echidna-test . --config <(cat echidna.yaml contracts/test/echidna/$TEST_CONTRACT.yaml) --contract $TEST_CONTRACT
}

echo "Fuzzing $1"

npx truffle compile --all && testsuite $1
