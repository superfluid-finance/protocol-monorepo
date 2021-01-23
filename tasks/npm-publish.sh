#!/bin/bash -e

D="$(dirname "$0")"

echo "Publishing $1 to NPMJS registry"
$D/npmrc-use-npmjs.sh > .npmrc
npm publish --access public --dry-run $1

echo "Publishing $1 to Github Packages"
$D/npmrc-use-github.sh > .npmrc
npm publish --access public --dry-run $1
