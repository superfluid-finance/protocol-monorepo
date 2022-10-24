#!/bin/sh

LHS_FILE=$(readlink -f "$1")
[ -f "$LHS_FILE" ] || exit 1

cd "$(dirname "$0")"/..

{
    cat $1
} | sed "1 d" | # remove emacs mode-line
    awk '
    /\\begin{haddock}/ { h = 1; next }
    /\\end{haddock}/   { h = 0; next }
    /\\begin{code}/    { if (h) next }
    /\\end{code}/      { if (h) next }
    /^{-/  { if (h) next }
    /^-}/  { if (h) next }
    /^--}/ { if (h) next }
    { print $0 }' | # process haddock documentation
    cat # nop, just to rhyme.
