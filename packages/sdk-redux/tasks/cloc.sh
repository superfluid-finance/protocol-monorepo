#!/bin/sh

# NOTE: let's make it compatible with BSD find (MacOS default)

cd "$(dirname "$0")"/..

CLOC="cloc --by-file-by-lang"

$CLOC src
