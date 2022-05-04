#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build contracts
yarn install --frozen-lockfile

# test contracts w/ forge
# yarn forge-test

npx hardhat compile

# test contracts w/ hardhat
yarn hardhat-test --network localhost
