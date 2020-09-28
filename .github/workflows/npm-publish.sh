#!/bin/bash -e

cd "$(dirname "$0")"/../..

git diff --quiet || { echo "There are unstaged local changes"; exit 1; }
git diff --cached --quiet || { echo "There are staged local changes"; exit 1; }

VERSION=`awk -F'"' '/"version": ".+"/{ print $4; exit; }' package.json`
TIME=`date -u +%Y%m%dT%H%M%SZ`
GIT_REV=`git rev-parse --short HEAD`

if echo "$VERSION" | fgrep -q -- "-latest";then
    PUBLISHING_VERSION=${VERSION%-latest*}-latest-${TIME}-${GIT_REV}

    jq -r ". |= . + {
        \"version\" : \"$PUBLISHING_VERSION\"
    }" package.json > package.json.new #&& \
        mv package.json.new package.json
else
    PUBLISHING_VERSION=VERSION
    echo "Publishing pacakge"
fi

echo "Publishing pacakge $PUBLISHING_VERSION"

npm publish

git checkout package.json
