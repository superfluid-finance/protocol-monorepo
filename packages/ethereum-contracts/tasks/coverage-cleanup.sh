#!/usr/bin/env bash

set -ex

cd "$(dirname "$0")"/..

# extract coverage for Superfluid contracts from forge coverage
lcov -e ../../lcov.info \
     "packages/ethereum-contracts/contracts/*" \
     -o lcov.info

# remove contracts whose coverage we don't care about (see .solcover.js)
lcov -r lcov.info \
     "packages/ethereum-contracts/contracts/mocks/*" \
     "packages/ethereum-contracts/contracts/apps/*Base*" \
     "packages/ethereum-contracts/contracts/utils/*Test*" \
     "packages/ethereum-contracts/contracts/utils/*Deploy*" \
     "packages/ethereum-contracts/contracts/apps/SuperfluidLoaderLibrary.sol" \
     -o lcov.info

# merge hardhat and forge coverage files
lcov -a lcov.info \
     -a coverage/lcov.info \
     -o coverage/lcov.info
