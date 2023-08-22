#!/usr/bin/env bash

set -e

# in a Nix shell, shellcheck is in PATH, else use the npm package
if command -v shellcheck > /dev/null 2>&1; then
    cd "$(dirname "$0")"/.. || exit 1

    # check .shellcheckrc
    find tasks packages/*/tasks -name '*.sh' -print0 \
        | xargs -0 -- shellcheck
elif [ -n "$CI" ]; then
    # CI is set to true per https://docs.github.com/en/actions/learn-github-actions/variables
    echo "ERROR: shellcheck is not installed, bailing." && exit 1
else
    echo "WARNING: shellcheck is not installed, skipping."
fi
