#!/bin/bash -e

echo "Publishing $1 to NPMJS registry"
./npmrc-use-npmjs.sh > .npmrc
npm publish --access public --dry-run $1

echo "Publishing $1 to Github Packages"
./npmrc-use-github.sh > .npmrc
npm publish --access public --dry-run $1
