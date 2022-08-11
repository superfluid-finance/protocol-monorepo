#!/bin/bash

# this script cleans up the hardhat build output so that only the .json artifacts remain
# it cleans up the build output so it is the same as if running npx truffle compile
path=$1

# remove directories in /build w/ .dbg.json files
find build -type d -mindepth 1 -maxdepth 1 ! -path "build/contracts" | while read i; do
    rm -rf $i
done

# remove directories in $path w/ .dbg.json files
find $path -type d -mindepth 1 -maxdepth 1 | while read i;do
    rm -rf $i
done