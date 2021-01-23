#!/bin/bash -e

D="$(dirname "$0")"

echo "Publishing $1 @$2 to NPMJS registry"
$D/npmrc-use-npmjs.sh > .npmrc
npm publish --tag $2 $1

echo "Publishing $1 @$2 to Github Packages"
$D/npmrc-use-github.sh > .npmrc
npm publish --tag $2 $1
