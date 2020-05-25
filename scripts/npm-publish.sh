#!/bin/bash

cd "$(dirname "$0")"/..

git diff --cached --quiet || echo "There are local changes" && exit

VERSION=`jq -r .version package.json`
TIME=`date -u +%Y%m%dT%H%M%SZ`
GIT_REV=`git rev-parse --short HEAD`

if echo "$VERSION" | fgrep -q -- "-latest";then
    PUBLISHING_VERSION=${VERSION}-${TIME}-${GIT_REV}

    jq -r ". |= . + {
        \"version\" : \"\"
    }" package.json > package.json.new #&& \
        mv package.json.new package.json
else
    PUBLISHING_VERSION=VERSION
    echo "Publishing pacakge"
fi

echo "Publishing pacakge $PUBLISHING_VERSION"

npm publish
