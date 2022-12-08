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

function setBuildAll() {
    BUILD_ETHEREUM_CONTRACTS=1
    BUILD_SDK_CORE=1
    BUILD_SDK_REDUX=1
    BUILD_SPEC_HASKELL=1
    BUILD_SUBGRAPH=1
    echo Everything will be tested.
}

# set BUILD_* variables to GITHUB_ENV
# (dependency graph implied below)
if ! [ -z "$GITHUB_ENV" ];then
    # if ci workflow changed
    if grep -E "^.github/workflows/ci.*.yml$" changed-files.list;then
        setBuildAll
    fi
    if grep -E "^.github/workflows/call.*.yml$" changed-files.list;then
        setBuildAll
    fi
    # if root package changed, rebuild everything
    if grep -E "^package.json$" changed-files.list;then
        setBuildAll
    fi
    # if ethereum-contracts package changed
    if grep -E "^packages/ethereum-contracts/(contracts/|scripts/|test/|truffle-config.js|package.json)" changed-files.list;then
        BUILD_ETHEREUM_CONTRACTS=1
        BUILD_SUBGRAPH=1
        echo Ethereum contracts and Subgraph will be tested.
    fi
    # if sdk-core package changed
    if grep -E "^packages/sdk-core/(src/|test/|package.json|tsconfig.*)" changed-files.list;then
        BUILD_SDK_CORE=1
        BUILD_SDK_REDUX=1
        BUILD_SUBGRAPH=1
        echo SDK-CORE, SDK-REDUX and SUBGRAPH will be tested.
    fi
    # if sdk-redux package changed
    if grep -E "^packages/sdk-redux/(src/|test/|package.json)" changed-files.list;then
        BUILD_SDK_REDUX=1
        echo SDK-REDUX will be tested.
    fi
    # if subgraph package changed
    if grep -E "^packages/subgraph/(subgraph.template.yaml|schema.graphql|config|scripts|src|tasks|test|hardhat.config.ts|package.json|docker-compose.yml)" changed-files.list;then
        BUILD_SUBGRAPH=1
        echo Subgraph will be tested.
    fi
    # if haskell spec package changed
    if grep -E "^packages/spec-haskell/(packages/|cabal.project)" changed-files.list;then
        BUILD_SPEC_HASKELL=1
        echo SPEC-HASKELL will be tested.
    fi

    echo "BUILD_ETHEREUM_CONTRACTS=${BUILD_ETHEREUM_CONTRACTS}" >> $GITHUB_ENV
    echo "BUILD_SDK_CORE=${BUILD_SDK_CORE}" >> $GITHUB_ENV
    echo "BUILD_SDK_REDUX=${BUILD_SDK_REDUX}" >> $GITHUB_ENV
    echo "BUILD_SUBGRAPH=${BUILD_SUBGRAPH}" >> $GITHUB_ENV
    echo "BUILD_SPEC_HASKELL=${BUILD_SPEC_HASKELL}" >> $GITHUB_ENV
    if [ "$BUILD_ETHEREUM_CONTRACTS" == 1 ] || [ "$BUILD_SDK_CORE" == 1 ] || [ "$BUILD_SDK_REDUX" == 1 ];then
        echo PR packages will be published.
        echo "PUBLISH_PR_ARTIFACT=1" >> $GITHUB_ENV
    fi
fi
