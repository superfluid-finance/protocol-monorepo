#!/usr/bin/env bash

# make sure that if any step fails, the script fails
set -xe

# if the typechain files do not exist, we build
# hardhat so that it does exist
if [ ! -d "../ethereum-contracts/typechain-types" ]; then
  cd ../ethereum-contracts
  yarn build:contracts:hardhat
  cd ../sdk-core
fi

# copy the typechain files over from ethereum-contracts
cp -r ../ethereum-contracts/typechain-types ./src/typechain-types-raw

# compile the typechain files in sdk-core
tsc -p tsconfig.typechain.json

# clean up the raw typechain files
rm -rf ./src/typechain-types-raw
