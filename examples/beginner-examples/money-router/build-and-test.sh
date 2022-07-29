#!/bin/bash
# make sure that permissions are correct on your local machine before running this script

# make sure that if any step fails, the script fails
set -xe

# build contracts
pnpm install --frozen-lockfile
yarn build

# test contracts
yarn test