#!/bin/bash

# This publishes a "canary" version of our packages to npm.
# It is run when a branch is merged to dev.
# https://github.com/lerna/lerna/tree/master/commands/publish#--canary

yarn lerna publish --canary --preid dev --dist-tag dev --yes
