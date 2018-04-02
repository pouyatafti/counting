#!/bin/sh

mkdir -p bin

ext="o hi hc"

for i in *.hs; do
	ghc --make -O2 -fexpose-all-unfoldings $i && mv ${i%.hs} bin/
	for e in $ext; do
		rm -f ${i%.hs}.$e
	done
done
