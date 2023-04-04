#!/usr/bin/env bash

cd "$(dirname "$0")"/.. || exit 1

CLOC="cloc --by-file-by-lang"

echo "========================================================="
echo "# Interfaces"
$CLOC contracts/interfaces

echo "========================================================="
echo "# Libs and Test Code"
$CLOC \
    contracts/{ux,apps}

echo "========================================================="
echo "# Excluding Interfaces, Libs and Test Code"
$CLOC \
    --exclude-dir interfaces,ux,apps,test,mocks \
    contracts
