#!/bin/bash

cd "$(dirname "$0")/.."

echo "Superfluid_ABI = module.exports = {"
for i in IERC20 FlowAgreement ISuperToken;do
    echo "    $i: $(jq -c '.abi' build/contracts/$i.json),"
done
echo "};"
