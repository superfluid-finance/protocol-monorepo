#!/bin/sh

cd "$(dirname "$0")"/.. || exit 1
CLOC="cloc --by-file-by-lang"

$CLOC \
    --not-match-f schema.graphql \
    --exclude-dir typechain,abi \
    --exclude-ext=generated.ts \
    src
