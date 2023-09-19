#!/usr/bin/env bash

# make sure that if any step fails, the script fails
set -xe

rm -rf ./src/typechain-types

# if the typechain files do not exist, we build
# hardhat so that it does exist
if [ ! -d "../ethereum-contracts/typechain-types" ]; then
  echo "typechain-types does not exist: You must build ethereum-contracts first to generate it."
  exit 1
fi

# copy the typechain files over from ethereum-contracts
cp -r ../ethereum-contracts/typechain-types ./src/typechain-types

# compile the typechain files in sdk-core
tsc -p tsconfig.typechain.json
