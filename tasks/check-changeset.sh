#!/bin/bash -e

SOURCE_REF=$1
BASE_REF=$2

# fetch the latest commit of the base ref
git fetch --depth=1 origin refs/heads/${BASE_REF}:refs/remotes/origin/${BASE_REF}
# compare the source branch with the dev branch
git diff --name-only ${SOURCE_REF} refs/remotes/origin/${BASE_REF} > changed-files.list
echo Changed files:
echo ---
cat changed-files.list
echo ---

# set BUILD_* variables to GITHUB_ENV
if ! [ -z "$GITHUB_ENV" ];then
    if grep -E "^packages/ethereum-contracts/(contracts/|scripts/|test/|truffle-config.js|package.json)" changed-files.list;then
        BUILD_ANYTHING=1
        BUILD_ETHEREUM_CONTRACTS=1
        BUILD_JS_SDK=1 # force js-sdk to be rebuilt to if contracts changed
        echo Ethereum contracts will be tested.
    fi
    if grep -E "^packages/js-sdk/(src/|scripts/|test/|truffle-config.js|package.json)" changed-files.list;then
        BUILD_ANYTHING=1
        BUILD_JS_SDK=1
        echo JS SDK will be tested.
    fi
    echo "BUILD_ANYTHING=${BUILD_ANYTHING}" >> $GITHUB_ENV
    echo "BUILD_ETHEREUM_CONTRACTS=${BUILD_ETHEREUM_CONTRACTS}" >> $GITHUB_ENV
    echo "BUILD_JS_SDK=${BUILD_JS_SDK}" >> $GITHUB_ENV
fi

echo "BUILD_ANYTHING=1" >> $GITHUB_ENV # FIXME testing
