#!/bin/bash

cd "$(dirname "$0")/.."

CONTRACTS="IERC20 TokenInfo TestResolver SuperfluidRegistry IFlowAgreement ISuperToken"

echo "if (typeof module === \"undefined\") module = {};"
echo "Superfluid_ABI = module.exports = {"
for i in $CONTRACTS;do
    echo "    $i: $(jq -c '.abi' build/contracts/$i.json),"
done
echo "};"
