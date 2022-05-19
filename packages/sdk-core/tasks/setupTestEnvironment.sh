#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build ABIs for local contract deployment
cd ../js-sdk
chmod +x ./tasks/build-abi-js.sh
./tasks/build-abi-js.sh

# uses built ABIs for local contract deployment
cd ../subgraph
yarn deploy-contracts-local

# Come back to sdk-core after going to subgraph folder
cd ../sdk-core
yarn generate:graphql-types
yarn generate:abi-files
yarn generate:web3-types