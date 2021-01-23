#!/bin/bash -e

echo "Publishing $1 to NPMJS registry"
./tasks/npm-use-npmjs.sh > .npmrc
npm publish --access public --dry-run $1

echo "Publishing $1 to Github Packages"
./tasks/npm-use-github.sh > .npmrc
npm publish --access public --dry-run $1
