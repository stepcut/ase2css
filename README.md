Adobe® Swatch Exchange (ASE) is a file format created by Adobe® that
allows users to easily share swatches. It is supported by many of the
tools in the Adobe Creative Suite®.

Users with accounts on http://kuler.adobe.com/ can also download
swatches in the ASE format.

This repository provides three things:

 1. a data type and `Binary` instance for parsing and generating ASE files

 2. a function for generating CSS from an ASE data type

 3. an executable 'ase2css' which parses a .ase file and generates a .css file


For each `ColorEntry` found in the `.ase` file, a pair of CSS rules are generated which look a bit like:

    /* Generated from "6 pm.ase" using ase2css */
    *.fg-color-ase-1 {
        color : #5c6c59 }
    
    *.bg-color-ase-1 {
        background-color : #5c6c59 }

the suffix -n is assigned in the order the colors appeared in the .ase file.

The .ASE format supports 4 color modes, RGB, CYMK, LAB, and Gray. The
`Binary` instance supports all four modes, but the `ase2css`
conversion function only supports `RGB` at this time.

This application is written in Haskell. If you are a Haskell user this
is great news! If you do not already have Haskell installed, it will
be a bit of a chore to install and build this package. Sorry there are
no binaries available!

If you want to try to build this package you will need to:

    1. install the Haskell Platform package available from http://www.haskell.org/platform/

    2. run `cabal update`

    3. run `cabal install ase2css`

