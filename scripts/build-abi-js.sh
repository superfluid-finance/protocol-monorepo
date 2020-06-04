#!/bin/bash

cd "$(dirname "$0")/.."

echo "if (typeof module === \"undefined\") module = {};"
echo "Superfluid_ABI = module.exports = {"
for i in IERC20 TestToken TestResolver IFlowAgreement ISuperToken;do
    echo "    $i: $(jq -c '.abi' build/contracts/$i.json),"
done
echo "};"
