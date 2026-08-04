#!/usr/bin/env bash
# Start the package testenv, run a command, then always stop the testenv.
# Usage (from a package with testenv:start / testenv:stop scripts):
#   ../../tasks/with-testenv.sh yarn test
set -Eeuo pipefail

cleanup() {
    test_status=$?
    trap - EXIT
    set +e
    yarn testenv:stop
    stop_status=$?
    if [ "$test_status" -ne 0 ]; then
        exit "$test_status"
    fi
    exit "$stop_status"
}

trap cleanup EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

yarn testenv:start
"$@"
