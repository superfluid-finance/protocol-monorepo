#!/bin/bash

# make sure that if any step fails, the script fails
set -xe

# build and test
yarn install --frozen-lockfile
yarn build
#yarn test
