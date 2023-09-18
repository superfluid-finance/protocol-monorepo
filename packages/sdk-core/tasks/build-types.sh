#!/usr/bin/env bash

# make sure that if any step fails, the script fails
set -xe

if [ -d "./src/typechain-types" ]; then
  rm -rf ./src/typechain-types
fi

# if the typechain files do not exist, we build
# hardhat so that it does exist
if [ ! -d "../ethereum-contracts/typechain-types" ]; then
  cd ../ethereum-contracts
  yarn build:contracts:hardhat
  cd ../sdk-core
fi

# copy the typechain files over from ethereum-contracts
cp -r ../ethereum-contracts/typechain-types ./src/typechain-types

# compile the typechain files in sdk-core
tsc -p tsconfig.typechain.json
