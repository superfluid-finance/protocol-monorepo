#!/bin/sh

cd "$(dirname "$0")"/..
CLOC="cloc --by-file-by-lang"

$CLOC --not-match-f 'abi.js' --by-file src 
