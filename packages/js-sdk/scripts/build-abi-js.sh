#!/bin/bash

cd "$(dirname "$0")/.."

which jq &>/dev/null || { echo "Install jq utility!" && exit 1; }

CONTRACTS=( $(jq -r .[] ./src/contracts.json) )

{
    echo "if (typeof module === \"undefined\") module = {};"
    echo "Superfluid_ABI = module.exports = {"
    for i in "${CONTRACTS[@]}";do
        echo "    $i: $(jq -c '.abi' ../ethereum-contracts/build/contracts/$i.json),"
    done
    echo "};"
} > scripts/abi.js
