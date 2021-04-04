#!/bin/bash

cd "$(dirname "$0")/.."

JQ="../../node_modules/node-jq/bin/jq"

CONTRACTS=( $($JQ -r .[] ./src/contracts.json) )
[ $? == 0 ] || exit 1

{
    echo "if (typeof module === \"undefined\") module = {};"
    echo "Superfluid_ABI = module.exports = {"
    for i in "${CONTRACTS[@]}";do
        ABI="$($JQ '.abi' ../ethereum-contracts/build/contracts/$i.json)"
        [ $? == 0 ] || exit 1
        echo "    $i: $ABI,"
    done
    echo "};"
} > src/abi.js
