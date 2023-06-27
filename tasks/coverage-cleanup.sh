#!/usr/bin/env bash

set -e

# extract coverage for NFT contracts from forge coverage
lcov -e lcov.info 'packages/ethereum-contracts/contracts/*' -o lcov.info
# remove mocks, base super app, test and deployer contracts (see .solcover.js)
lcov -r lcov.info 'packages/ethereum-contracts/contracts/mocks/*' 'packages/ethereum-contracts/contracts/apps/*Base*' 'packages/ethereum-contracts/contracts/utils/*Test*' 'packages/ethereum-contracts/contracts/utils/*Deploy*' 'packages/ethereum-contracts/contracts/apps/SuperfluidLoaderLibrary.sol' -o lcov.info
# merge hardhat and forge coverage files
lcov -a lcov.info -a packages/ethereum-contracts/coverage/lcov.info -o packages/ethereum-contracts/coverage/lcov.info
