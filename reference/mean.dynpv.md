# Mean present value per uptaking patient

Mean of the Present Value per uptaking patient, by time at which the
calculation is performed (`tzero` input to
[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)).

## Usage

``` r
# S3 method for class 'dynpv'
mean(x, ...)
```

## Arguments

- x:

  Tibble of class "dynpv" created by
  [`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
  or
  [`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)

- ...:

  Currently unused

## Value

A number or tibble

## Details

This is equal to
[`total()`](https://MSDLLCpapers.github.io/dynacem/reference/total.md)
divided by
[`uptake()`](https://MSDLLCpapers.github.io/dynacem/reference/uptake.md).

## See also

[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md),
[`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)
