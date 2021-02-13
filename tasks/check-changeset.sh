#!/bin/bash -e

SOURCE_REF=$1
BASE_REF=$2

echo SOURCE_REF: $SOURCE_REF
echo BASE_REF: $BASE_REF

# fetch the latest commit of the base ref
git fetch origin --depth=1 refs/heads/${BASE_REF}:refs/remotes/origin/${BASE_REF}

# compare the source branch with the dev branch
git diff --name-only ${SOURCE_REF} refs/remotes/origin/${BASE_REF} > changed-files.list
echo Changed files:
echo ---
cat changed-files.list
echo ---

# set BUILD_* variables to GITHUB_ENV
if ! [ -z "$GITHUB_ENV" ];then
    # if ci workflow changed
    if grep -E "^.github/workflows/ci.hml$" changed-files.list;then
        BUILD_ETHEREUM_CONTRACTS=1
        BUILD_JS_SDK=1
    fi
    # if ethereum-contracts package changed
    if grep -E "^packages/ethereum-contracts/(contracts/|scripts/|test/|truffle-config.js|package.json)" changed-files.list;then
        BUILD_ETHEREUM_CONTRACTS=1
        BUILD_JS_SDK=1 # force js-sdk to be rebuilt to if contracts changed
        echo Ethereum contracts will be tested.
    fi
    # if js-sdk package changed
    if grep -E "^packages/js-sdk/(src/|scripts/|test/|truffle-config.js|package.json)" changed-files.list;then
        BUILD_JS_SDK=1
        echo JS SDK will be tested.
    fi
    # if any exapmle project changed
    if grep -E "^examples/" changed-files.list;then
        BUILD_EXAMPLES=1
        echo Examples will be tested.
    fi
    echo "BUILD_ETHEREUM_CONTRACTS=${BUILD_ETHEREUM_CONTRACTS}" >> $GITHUB_ENV
    echo "BUILD_EXAMPLES=${BUILD_EXAMPLES}" >> $GITHUB_ENV
    echo "BUILD_JS_SDK=${BUILD_JS_SDK}" >> $GITHUB_ENV
    if [ "$BUILD_ETHEREUM_CONTRACTS" == 1 || "$BUILD_JS_SDK" == 1];then
        echo PR packages will be published.
        echo "PUBLISH_PR_ARTIFACT=1" >> $GITHUB_ENV
    fi
fi
