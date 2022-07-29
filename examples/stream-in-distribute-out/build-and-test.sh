#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build contracts
pnpm install --frozen-lockfile
yarn build

# test contracts
yarn test