#!/bin/sh

cd "$(dirname "$0")"/..

echo "# Interfaces"
find contracts/interfaces/ -name '*.sol' | xargs wc -l

echo

echo "# Excluding Interfaces and Test Code"
find contracts \
    -not -path 'contracts/interfaces/*' \
    -not -path 'contracts/test/*' \
    -not -path 'contracts/mocks/*' \
    -name '*.sol' | xargs wc -l
