#!/bin/sh

PROF="-prof -fprof-auto -fprof-cafs"
OPTS="-dynamic -Wall -threaded" # $PROF"

OUT="4ch50replies"
CLEAN=false
for arg; do
	[ "$arg" = clean ] && { CLEAN=true; break; }
	OUT="$arg"
done

if "$CLEAN"; then
	rm -fr ./**/*.hi ./**/*.o
	exit 0
fi

ghc $OPTS -iapp app/Main.hs --make -o "$OUT"
