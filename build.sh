#!/bin/bash

if [ "$1" = "--debug" ]; then debug="-DDEBUG"; fi

if [ ! -d build/assets/gfx ]; then mkdir -p build/assets/gfx; fi
cp src_assets/gfx/* build/assets/gfx

if [ ! -d build/assets/levels ]; then mkdir -p build/assets/levels; fi
cp src_assets/levels/* build/assets/levels

if [ ! -d build ]; then mkdir build; fi
for exe in snake; do
	ghc -cpp -DASSET_PREFIX="\"./assets/\"" $debug -ibuild:src -outputdir build --make src/$exe.hs -o build/$exe
done

