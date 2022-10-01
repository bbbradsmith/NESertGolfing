# NESert Golfing FDS

A 2D golf game for the NES, directly inspired by [Desert Golfing](https://captaingames.itch.io/desert-golfing).

This is an alternate version of
[NESert Golfing](https://github.com/bbbradsmith/NESertGolfing)
for the Famicom Disk System.

Brad Smith, 2019.

Official website:
https://rainwarrior.itch.io/nesert-golfing

Author's website:
http://rainwarrior.ca

## License

This source code is provided under the Creative Commons Attribution license. (CC BY 4.0)

Full details of this license are available here:
https://creativecommons.org/licenses/by/4.0/

This approximately means that you are free to reuse this source code for your own purposes,
provided that you include an attribution to me (Brad Smith) in documentation and
accessible credits for the work it is used in.

## Prerequisites

The only required tool for building this game is CC65:
https://cc65.github.io/

The specific version of CC65 used:
**cc65 V2.17 - Git 80a43d7**

Optional:
- [Mesen](https://www.mesen.ca/) - An NES emulator and debugger.
- [FCEUX](http://www.fceux.com/) - An alternative NES emulator and debugger.
- [Python 3.7](https://www.python.org/) - Used for some included build scripts.
- [Aseprite](https://www.aseprite.org/) - Used to prototype animated sprites.
- [FamiTracker](http://famitracker.com/) - Used to prototype sounds.

The provided build script (*build.bat*) is a Windows batch file.
If you wish to build this program on another platform,
you will have to recreate a similar script for yourself in a suitable format.

## Guide

Place CC65 in a *cc65/* directory in the root of this project.
The CC65 toolchain executables should be in *cc65/bin/*.

The main code for this game is contained in *dgolf.c* and *dgolf.s*.
These are two separate but complementary halves of the source code.
The C side contains most of the high level logic,
and the assembly (*.s*) side contains low level hardware access
and some other routines that needed to be written
directly in more efficient assembly.

The output NES ROM *temp/dgolf.nes* is built by running *build.bat*.

The build will also create a debug symbols file *temp/dgolf.deb*,
which can be used with the Mesen emulator.

Some additional assembly code is contained in *blend.s*.
I made this separately as a generic routine to blend between any
two colours in the NES palette in 4 steps.

The graphics tiles are contained in *layers.chr* and *sprite.chr*,
but these files were built from the correspoding *.png* images.
You can use *build_data.py* to rebuild the *.chr* files from
the source images.

A subset of the CC65 runtime libraries have been compiled
into *temp/runtime.lib*, but this may be rebuilt if you wish
to use a later version of its libraries.
Read the comments in *build_runtime.bat* for details.

A few other files have been included that aren't directly
used as part of the build. See the file list below for details.

## Files

- *build.bat* - Batch file to build *temp/dgolf.nes*.
- *build_runtime.bat* - Batch file to rebuild *temp/runtime.lib*.
- *build_data.py* - Python script to build *layers.chr*, *layerste.chr*, *sprite.chr*, and *temp/slopes.inc*.
- *fmult.py* - Python script for prototyping *_fmult* subroutine found in *dgolf.s*.
- *dgolf.c* - C code for NESert Golfing.
- *dgolf.s* - Assembly code for NESert Golfing.
- *blend.s* - Assembly code generic 4-step NES colour blend.
- *dgolf.cfg* - ROM build layout for CC65 linker.
- *layer1.png* - Title image tiles, monochrome.
- *layer1te.png* - Tournament Edition title image tiles, monochrome.
- *layer2.png* - Font set tiles, monochrome.
- *sprite.png* - Sprite tiles, 3 colour.
- *layers.chr* - Compiled from *layer1.png* and *layer2.png*.
- *layerste.chr* - Compiled from *layer1te.png* and *layer2.png*.
- *sprite.chr* - Compiled from *sprite.png*.
- *sounds.ftm* - FamiTracker prototype of the sound effects used in the game.
- *splash.ase* - Aseprite animation prototype of the ball splashing into the sea.
- *temp/dgolf.nes* - Provided game ROM, already built.
- *temp/runtime.lib* - Pre-compiled CC65 runtime library.
- *temp/slopes.inc* - Generated slope tables.

## Other Versions

Tournament Edition:
- In version 1.5 this was merged into the regular game. Select HELP from the menu, press and hold A, then press SELECT to switch to Tournament Edition. (A+B+SELECT+START will also reset the game.)
