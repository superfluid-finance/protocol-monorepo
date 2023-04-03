#!/usr/bin/env bash
# shellcheck disable=SC2207

cd "$(dirname "$0")/.." || exit 1

JQ="npx --package=node-jq -- jq"

CONTRACTS=( $($JQ -r .[] ./src/contracts.json) ) || exit 2

{
    echo "if (typeof module === \"undefined\") module = {};"
    echo "// eslint-disable-next-line no-unused-vars"
    echo "let Superfluid_ABI;"
    echo "Superfluid_ABI = module.exports = {"
    for i in "${CONTRACTS[@]}";do
        ABI="$($JQ ".abi" ../ethereum-contracts/build/contracts/"$i".json)" || exit 3
        echo "    $i: $ABI,"
    done
    echo "};"
} > src/abi.js
