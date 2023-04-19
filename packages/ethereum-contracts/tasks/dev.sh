#!/usr/bin/env bash

# It will use nodemon to monitor changes and reload test for TDD experience.
# And:
# if parameters is provided, they are considered testsuites and will be run using hardhat test,
# otherwise full testsuite is run.

cd "$(dirname "$0")"/.. || exit 1

export IS_HARDHAT=true

C="$*"

if [ -z "$C" ];then
    X="yarn test:contracts:hardhat 2>&1"
else
    X="yarn run-hardhat test $C"
fi

yarn run-nodemon "$X"
