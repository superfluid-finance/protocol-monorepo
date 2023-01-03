#!/usr/bin/env bash

# make sure that if any step fails, the script fails
set -xe

yarn generate:graphql-types
# compile contracts before starting tests to generate typechain folder
npx hardhat compile
