# Calculate present value for a payoff in a single cohort with dynamic pricing across multiple timepoints

Present value of a series of payoffs for a single given cohort, entering
at given future time, allowing for dynamic pricing. This function is a
wrapper for
[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
restricted to evaluation of a single cohort.

## Usage

``` r
futurepv(tzero = 0, payoffs, prices, discrate)
```

## Arguments

- tzero:

  Time at the date of calculation, to be used in lookup in prices vector

- payoffs:

  Vector of payoffs of interest (numeric vector)

- prices:

  Vector of price indices through the time horizon of interest

- discrate:

  Discount rate per timestep, corresponding to price index

## Value

A tibble of class "dynpv" with the following columns:

- `j`: Time at which patients began treatment

- `k`: Time since patients began treatment

- `l`: Time offset for the price index (from `tzero`)

- `t`: Equals \\j+k-1\\

- `uj`: Uptake of patients beginning treatment at time \\j\\ (from
  `uptakes`)

- `pk`: Cashflow amount in today's money in respect of patients at time
  \\k\\ since starting treatment (from `payoffs`)

- `R`: Index of prices over time \\l+t\\ (from `prices`)

- `v`: Discounting factors, \\(1+i)^{1-t}\\, where `i` is the discount
  rate per timestep

- `pv`: Present value, \\PV(j,k,l)\\

## See also

[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

# Obtain dataset
democe <- get_dynfields(
   heemodel = oncpsm,
   payoffs = c("cost_daq_new", "cost_total", "qaly"),
   discount = "disc"
   )

# Obtain discount rate
discrate <- get_param_value(oncpsm, "disc")

# Obtain payoff vector of interest
payoffs <- democe |>
   filter(int=="new") |>
   mutate(cost_oth_rup = cost_total_rup - cost_daq_new_rup)
Nt <- nrow(payoffs)

# Run calculation for times 0-9
fpv <- futurepv(
  tzero = (0:9)*52,
  payoffs = payoffs$cost_oth_rup,
  prices = 1.001^(1:(2*Nt)-1), # Approx 5.3% every 52 steps
  discrate = 0.001 + discrate
)
fpv
#> # A tibble: 10,440 × 9
#>        j     k     l     t    uj    pk     R     v    pv
#>    <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1     1     0     1     1  355.  1        1  355.
#>  2     1     1    52     1     1  355.  1.05     1  374.
#>  3     1     1   104     1     1  355.  1.11     1  394.
#>  4     1     1   156     1     1  355.  1.17     1  415.
#>  5     1     1   208     1     1  355.  1.23     1  437.
#>  6     1     1   260     1     1  355.  1.30     1  460.
#>  7     1     1   312     1     1  355.  1.37     1  485.
#>  8     1     1   364     1     1  355.  1.44     1  511.
#>  9     1     1   416     1     1  355.  1.52     1  538.
#> 10     1     1   468     1     1  355.  1.60     1  566.
#> # ℹ 10,430 more rows
summary(fpv)
#> Summary of Dynamic Pricing and Uptake
#>      Number of cohorts:             1044 
#>      Number of times:               10 
#>      Total uptake:                  1 
#> 
#>  Total and mean present values by timepoint: 
#> # A tibble: 10 × 3
#>    tzero  total   mean
#>    <dbl>  <dbl>  <dbl>
#>  1     0 52165. 52165.
#>  2    52 54948. 54948.
#>  3   104 57880. 57880.
#>  4   156 60967. 60967.
#>  5   208 64220. 64220.
#>  6   260 67646. 67646.
#>  7   312 71255. 71255.
#>  8   364 75056. 75056.
#>  9   416 79060. 79060.
#> 10   468 83278. 83278.
```
