#!/bin/sh

# NOTE: let's make it compatible with BSD find (MacOS default)

cd "$(dirname "$0")"/.. || exit 1

CLOC="cloc --by-file-by-lang"

$CLOC src
