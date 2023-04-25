#!/usr/bin/env bash

set -e

# in a Nix shell, shellcheck is in PATH, else use the npm package
if command -v shellcheck > /dev/null 2>&1; then
  cd "$(dirname "$0")"/.. || exit 1

  # shellcheck disable=SC2086
  find tasks packages/*/tasks -name '*.sh' -print0 \
      | xargs -0 -- shellcheck -s bash
else
  echo "WARNING: shellcheck is not installed, skipping"
fi


