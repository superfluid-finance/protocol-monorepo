#!/bin/sh

cd "$(dirname "$0")"/..

LHS_FILE=$1
[ -f "$LHS_FILE" ] || exit 1

TEX_FILE="$(basename $LHS_FILE)"

{
    cat $1
} | \
    sed -e '/\s*--.*/d' | # quirk: fix for haskell comments
    sed -e 's/\\begin{code}/\\begin{minted}{haskell}/g' -e 's/\\end{code}/\\end{minted}/g' | # quirk: fix for haskell comments
    cat
