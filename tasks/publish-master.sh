#!/bin/bash -e

echo Publishing to NPMJS registry
./tasks/npm-use-npmjs.sh > .npmrc
npm publish --access public --dry-run

echo Publishing to Github Packages
./tasks/npm-use-github.sh > .npmrc
npm publish --access public --dry-run
