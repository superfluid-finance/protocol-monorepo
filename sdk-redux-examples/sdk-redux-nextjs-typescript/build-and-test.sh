#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build and test
yarn install --immutable
yarn build
#yarn test
