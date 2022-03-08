#!/bin/sh

cd "$(dirname "$0")"/..

CLOC="cloc --by-file-by-lang"

echo "# Interfaces"
$CLOC contracts/interfaces

echo "========================================================="

echo "# Excluding Interfaces and Test Code"
$CLOC \
    --exclude-dir interfaces,test,mocks \
    contracts
