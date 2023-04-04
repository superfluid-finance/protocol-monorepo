#!/usr/bin/env bash

set -xe

SHELLCHECK="npx shellcheck"

cd "$(dirname "$0")"/.. || exit 1

# shellcheck disable=SC2086
find tasks packages/*/tasks -name '*.sh' -print0 \
    | xargs --null -- $SHELLCHECK -s bash
