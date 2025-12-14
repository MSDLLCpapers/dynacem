# Summarize a dynpv object

Summarize a dynpv object

## Usage

``` r
# S3 method for class 'dynpv'
summary(object, ...)
```

## Arguments

- object:

  Tibble of class "dynpv" created by
  [`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
  or
  [`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)

- ...:

  Currently unused

## Value

A list of class "dynpv_summary" with the following elements:

- `ncoh`: Number of cohorts of uptaking patients, from
  [`ncoh()`](https://MSDLLCpapers.github.io/dynacem/reference/ncoh.md)

- `ntimes`: Number of times at which present value calculations are
  performed, from
  [`ntimes()`](https://MSDLLCpapers.github.io/dynacem/reference/ntimes.md)

- `uptake`: Total number of uptaking patients, from
  [`uptake()`](https://MSDLLCpapers.github.io/dynacem/reference/uptake.md)

- `sum_by_coh`: Present value for each uptake cohort and calculation
  time, from
  [`sum_by_coh()`](https://MSDLLCpapers.github.io/dynacem/reference/sum_by_coh.md)

- `total`: Total present value, from
  [`total()`](https://MSDLLCpapers.github.io/dynacem/reference/total.md)

- `mean`: Mean present value per uptaking patient, from
  [`mean()`](https://rdrr.io/r/base/mean.html), equal to
  `total`/`uptake`.

## See also

[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md),
[`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)
