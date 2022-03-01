#!/bin/bash -xe

D="$(dirname "$0")"

pwd

PACKAGE_DIR="$1"
TAG="$2"
shift 2

echo "Publishing ${PACKAGE_DIR} @${TAG} to NPMJS registry"
$D/npmrc-use-npmjs.sh > .npmrc
npm publish --access public --tag ${TAG} ${PACKAGE_DIR} "$@"

echo "Publishing ${PACKAGE_DIR} @${TAG} to Github Packages"
$D/npmrc-use-github.sh > .npmrc
npm publish --access public --tag ${TAG} ${PACKAGE_DIR} "$@"
