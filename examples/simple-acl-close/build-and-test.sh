#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build contracts
pnpm install --frozen-lockfile

# set up hardhat node
yarn hardhat-node &

# test contracts w/ forge
# yarn forge-test

npx hardhat compile

# test contracts w/ hardhat
yarn hardhat-test --network localhost
