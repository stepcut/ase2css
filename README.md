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
