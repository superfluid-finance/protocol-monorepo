#!/bin/bash

cd "$(dirname "$0")/.."

JQ="../../node_modules/node-jq/bin/jq"

CONTRACTS=( $($JQ -r .[] ./src/contracts.json) )
[ $? == 0 ] || exit 1

{
    for i in "${CONTRACTS[@]}";do
        ABI="$($JQ '.abi' ../ethereum-contracts/build/contracts/$i.json)"
        [ $? == 0 ] || exit 1
        echo "{ \"abi\": $ABI }" > src/abi/$i.json
    done
}
