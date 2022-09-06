#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# uses built ABIs for local contract deployment
cd ../subgraph
yarn deploy-contracts-local

# Come back to sdk-core after going to subgraph folder
cd ../sdk-core
yarn generate:graphql-types
yarn generate:abi-files
yarn generate:web3-types
# compile contracts before starting tests to generate typechain folder
npx hardhat compile