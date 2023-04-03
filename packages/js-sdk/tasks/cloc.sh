#!/bin/sh

cd "$(dirname "$0")"/.. || exit 1
CLOC="cloc --by-file-by-lang"

$CLOC --not-match-f 'abi.js' --by-file src 
