#!/bin/bash

cd "$(dirname "$0")/.."

echo "starting consistency tests";
echo "Cleaning ABI";
rm -rf build/contracts/*.js;
truffle build;

for i in `seq $1`;
    do
        echo "$i/$1...";
        # truffle test ../test/*.js;
        truffle test test/*.js | grep "Contract\|#\|Error";
done
