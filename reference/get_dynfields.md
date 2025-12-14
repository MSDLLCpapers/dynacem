# Helper function to get a tibble of the relevant fields from heemod output

Helper function to get a tibble of the relevant fields from heemod
output

## Usage

``` r
get_dynfields(heemodel, payoffs, discount, fname = NA)
```

## Arguments

- heemodel:

  A health economic model object from the *heemod* package (see
  [heemod::heemod-package](https://aphp.github.io/heemod/reference/heemod-package.html)).

- payoffs:

  Field names of payoffs of interest (string vector)

- discount:

  Name of parameter providing discount rate per cycle (string)

- fname:

  Export data to a .CSV file of this name, if given (character)

## Value

Tibble of payoffs taken from the heemod model, by intervention and model
timestep (`model_time`).

The field `vt` is calculated as `(1+i)^(1-model_time)`, where `i` is the
discount rate per model timestep set in the *heemod* model through the
parameter `disc_cycle`. This can be useful in 'rolling-up' payoff values
to the timestep in which they were incurred.

An additional set of payoffs (identified with a "\_rup" suffix) provides
calculations of the payoffs as at the start of the timestep in which
they were incurred, i.e. original payoff / `vt`.

## See also

[heemod::heemod-package](https://aphp.github.io/heemod/reference/heemod-package.html)

## Examples

``` r
democe <- get_dynfields(
   heemodel = oncpsm,
   payoffs = c("cost_daq_new", "cost_total", "qaly"),
   discount = "disc"
   )
head(democe)
#> # A tibble: 6 × 9
#>   model_time cost_daq_new cost_total   qaly int      vt cost_daq_new_rup
#>        <int>        <dbl>      <dbl>  <dbl> <chr> <dbl>            <dbl>
#> 1          1            0       695. 0.0153 soc   1                    0
#> 2          2            0       705. 0.0152 soc   0.999                0
#> 3          3            0       714. 0.0151 soc   0.999                0
#> 4          4            0       721. 0.0150 soc   0.998                0
#> 5          5            0       726. 0.0149 soc   0.998                0
#> 6          6            0       730. 0.0148 soc   0.997                0
#> # ℹ 2 more variables: cost_total_rup <dbl>, qaly_rup <dbl>
```
