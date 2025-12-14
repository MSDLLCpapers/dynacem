# Method to add two dynpv objects together

Add together two objects each of S3 class "dynpv": e1 + mult \* e2

## Usage

``` r
addprod(e1, e2, mult)
```

## Arguments

- e1:

  First "dynpv" object

- e2:

  Second "dynpv" object

- mult:

  Numeric

  Present value is present value from `e1` plus `mult` times the present
  value from `e2`. Total uptake is the uptake from `e1` plus `mult`
  times the uptake from `e2`. Take care of this when using `$mean` of
  the summed object.

## Value

S3 object of class "dynpv"

## See also

[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md),
[`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)
