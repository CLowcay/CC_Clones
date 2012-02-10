#!/bin/bash

for opt in $* ; do
	if [ "$opt" = "--debug" ]; then debug="-DDEBUG"; fi
	if [ "$opt" = "--cheating" ]; then cheating="-DCHEATING"; fi
done

if [ ! -d build/assets/gfx ]; then mkdir -p build/assets/gfx; fi
cp src_assets/gfx/* build/assets/gfx

if [ ! -d build/assets/levels ]; then mkdir -p build/assets/levels; fi
cp src_assets/levels/* build/assets/levels

if [ ! -d build/assets/sfx ]; then mkdir -p build/assets/sfx; fi
cp src_assets/sfx/* build/assets/sfx

if [ ! -d build/assets/fonts ]; then mkdir -p build/assets/fonts; fi
cp src_assets/fonts/titillium/*.otf build/assets/fonts

cp src_assets/highscores build/assets

if [ ! -d build ]; then mkdir build; fi
for exe in snake tetris; do
	/usr/bin/windres "src/${exe}.rc" "build/${exe}res.o"

	ghc -cpp -DASSET_PREFIX="\"./assets/\"" $debug $cheating \
		-ibuild:src -outputdir build \
		-L"C:\SDL-1.2.14\lib" -lSDL_ttf -lSDL_mixer -optl-mwindows \
		-optl"build/${exe}res.o" -O2 \
		--make src/$exe.hs -o build/$exe
done


