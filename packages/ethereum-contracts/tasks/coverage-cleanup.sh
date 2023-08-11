#!/usr/bin/env bash

set -ex

cd "$(dirname "$0")"/..

# extract coverage for NFT contracts from forge coverage
lcov -e ../../lcov.info \
     'contracts/*' \
     -o lcov.info

# remove mocks, base super app, test and deployer contracts (see .solcover.js)
lcov -r lcov.info \
     'contracts/mocks/*' \
     'contracts/apps/*Base*' \
     'contracts/utils/*Test*' \
     'contracts/utils/*Deploy*' \
     'contracts/apps/SuperfluidLoaderLibrary.sol' \
     -o lcov.info

# merge hardhat and forge coverage files
lcov -a lcov.info \
     -a coverage/lcov.info \
     -o coverage/lcov.info
