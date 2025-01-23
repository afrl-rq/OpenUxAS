# Visibility Graph #

## Top Level Function ##
The top level funcdtion for this library can be found in the `operating_region` package and is:
`function processOperatingRegion(region: Polygon_Set) return FSFS.FS.Finseq`

This function takes a `Polygon_Set` (also defined in `operating_region`) and returns a list (or finite sequence) of line segments that corresponds to the visibility graph.

A quick set of notes on the unusual name of the return type:
* FS is an abbreviation for "finite sequence" and corresponds to the finite sequences theory defined in the PVS prelude
* FSF is an abbreviation for "finite sequence functions" and corresponds to an auxilliary theory we developed for this effort
* However, FS and FSF correspond to templated SPARK Ada packages, so FSFS is an instantiation of that template for segments.
* So, FSFS.FS.Finseq corresponds to the finite sequence type in the finite sequence template in the finite sequence function template, instantiated for segments.

A `Polygon_Set` is a finite sequence of zone pairs, where a zone pair is a unique ID combined with a zone_polygon. The `zone_polygon` type is defined in the `zone_polygons` package.

## PVS ##
Most of the PVS files that the SPARK Ada code is based on can be found in the NASA PVS libraries at `https://github.com/nasa/pvslib`. The directories containing the PVS theories corresponding to the SPARK Ada code are primarily:
* `line_segments`
* `polygons`
* `polygon_merge`

Eventually, we expect that the remaining PVS files will also reside in the NASA PVS libraries, though they will be located in one or more additional directdories.

## Successful Proving Environment ##
Most VCs should discharge for this directory when using the following command:
```bash
gnatprove -P%PP -j0 %X --output=oneline --ide-progress-bar --level=4 --prover=cvc5,z3,altergo,colibri,cvc4
```
and when using the following version of gnatprove:
```
SPARK Pro 24.0w (20230509)
Why3 for gnatprove version 1.6.0+git
/opt/gnatstudio/libexec/spark/bin/alt-ergo: Alt-Ergo version 2.4.0
/opt/gnatstudio/libexec/spark/bin/colibri: Colibri 2020.9
/opt/gnatstudio/libexec/spark/bin/cvc5: This is CVC5 version 1.0.5
/opt/gnatstudio/libexec/spark/bin/z3: Z3 version 4.11.2 - 64 bit
```

## Publications ##

The publications corresponding to polygon merge are primarily:

1. Hocking, A.B., Di Vito, B.L. and Rowanhill, J.C., 2024, September. *From Formal Specification to Verified Implementation*. In 2024 AIAA DATC/IEEE 43rd Digital Avionics Systems Conference (DASC). IEEE.
2. Di Vito, B.L. and Hocking, A.B., 2021, May. *Polygon Merge: A Geometric Algorithm Verified Using PVS*. In NASA Formal Methods Symposium (pp. 79-94). Cham: Springer International Publishing.