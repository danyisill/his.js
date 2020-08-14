#!/bin/sh

OUT="4ch50replies"
[ -n "$1" ] && OUT="$1"

ghc -dynamic -isrc src/Main.hs --make -o "$OUT"
