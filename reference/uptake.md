# Total number of uptaking patients

Total number of uptaking patients, calculated as the sum of the `uptake`
input to
[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md),
or \\\sum\_{j=1}^T u_j\\

## Usage

``` r
uptake(df)
```

## Arguments

- df:

  Tibble of class "dynpv" created by
  [`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
  or
  [`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)

## Value

A number or tibble

## See also

[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md),
[`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)
