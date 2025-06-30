
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynacem: Evaluate cost-effectiveness models with dynamic pricing and uptake <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

The goal of *dynacem* is to evaluates cost-effectiveness models with
dynamic pricing and uptake.

Cost-effectiveness models are conventionally developed for single
cohorts without considering of lifecycle drug pricing or other causes of
dynamic pricing. This package allows calculation of cost-effectiveness
results after applying dynamic pricing and dynamic uptake assumptions.
The starting point is a conventional cohort cost-effectiveness model,
such as one computed using the
[heemod](https://cran.r-project.org/package=heemod) package.

## Installation

You can install the development version of dynacem from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
# pak::pak("MSDLLCpapers/dynacem")
```

## Example

Some example present value calculations are provided below for a single
payoff. A typical cost-effectiveness model may involve several separate
payoffs, for each intevention modeled, each with their own price index
and discount rate.

``` r
# Load the dynacem package
library(dynacem)
library(ggplot2)

# Review oncpsm model (heemod object)
oncpsm
#> 2 strategies run for 1044 cycles.
#> 
#> Initial state counts:
#> 
#> PF = 1
#> PD = 0
#> Death = 0
#> 
#> Counting method: 'life-table'.
#> 
#> Values:
#> 
#>     cost_daq_soc cost_daq_new cost_dadmin cost_dman   cost_ae cost_subs
#> soc     19455.25          0.0    2431.906  4601.624  8000.267  42634.43
#> new         0.00     141997.2    7099.860  8666.132 19999.582  16394.22
#>     cost_total   pf_year life_year     qaly
#> soc   77123.47 0.9582797  1.639185 1.154261
#> new  194156.99 1.9164555  2.963770 2.079786
#> 
#> Efficiency frontier:
#> 
#> soc -> new
#> 
#> Differences:
#> 
#>     Cost Diff. Effect Diff.   ICER Ref.
#> new   117033.5    0.9255249 126451  soc

# Derive the dataset necessary for the dynamic pricing/uptake calculations
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

# Obtain a vector of payoffs
payoffs <- democe |>
    dplyr::filter(int=="new") |>
    dplyr::mutate(cost_oth = cost_total - cost_daq_new)
payoffs
#> # A tibble: 1,044 × 10
#>    model_time cost_daq_new cost_total   qaly int      vt cost_daq_new_rup
#>         <int>        <dbl>      <dbl>  <dbl> <chr> <dbl>            <dbl>
#>  1          1        1493.      1847. 0.0153 new   1                1493.
#>  2          2        1477.      1831. 0.0153 new   0.999            1478.
#>  3          3        1461.      1815. 0.0152 new   0.999            1463.
#>  4          4        1446.      1799. 0.0152 new   0.998            1448.
#>  5          5        1431.      1783. 0.0151 new   0.998            1434.
#>  6          6        1416.      1766. 0.0150 new   0.997            1420.
#>  7          7        1401.      1750. 0.0149 new   0.997            1406.
#>  8          8        1386.      1733. 0.0148 new   0.996            1392.
#>  9          9        1372.      1717. 0.0147 new   0.995            1378.
#> 10         10        1357.      1700. 0.0146 new   0.995            1364.
#> # ℹ 1,034 more rows
#> # ℹ 3 more variables: cost_total_rup <dbl>, qaly_rup <dbl>, cost_oth <dbl>
```

Now let us calculate a discounted present value, given dynamic uptake
(of one patient per timestep) and dynamic pricing. We assume that there
are 52 timesteps per year and a discount rate of 3% (real) per year. The
general rate of inflation is 5% per year. The underlying price of the
payoff being costed rises with inflation of 5% for the first three
years, then drops by 50%, where it rises by 4% per year.

``` r
# Time horizon
Nt <- nrow(payoffs)

# Nominal discount rate
disc_py <- 0.03 + 0.05
disc_pt <- (1+disc_py)^(1/52) - 1

# Price index
prices <- c(1.05^((1:156)/52), 0.5 * (1.05^3) * 1.04^((1:(2 * Nt-156))/52))

# Graphically check the prices index
dsprices <- tibble::tibble(
  years = (1:Nt)/52,
  prices = prices[1:Nt]
)
ggplot2::ggplot(dsprices, aes(x = years, y = prices)) +
  geom_line()
```

<img src="man/figures/README-calc1-1.png" width="100%" />

``` r

# Calculate total discounted present value, given dynamic uptake and pricing
pv1 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices[1:Nt],
    disc = disc_pt
    )
pv1$results
#> $ncoh
#> [1] 1044
#> 
#> $uptake
#> [1] 1044
#> 
#> $calc
#> # A tibble: 545,490 × 9
#>        j     k     l     t    uj    pk     R     v    pv
#>    <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1     1     0     1     1  355.  1.00 1      355.
#>  2     1     2     0     2     1  354.  1.00 0.999  354.
#>  3     1     3     0     3     1  354.  1.00 0.997  354.
#>  4     1     4     0     4     1  353.  1.00 0.996  353.
#>  5     1     5     0     5     1  352.  1.00 0.994  351.
#>  6     1     6     0     6     1  351.  1.01 0.993  350.
#>  7     1     7     0     7     1  349.  1.01 0.991  348.
#>  8     1     8     0     8     1  347.  1.01 0.990  346.
#>  9     1     9     0     9     1  345.  1.01 0.988  344.
#> 10     1    10     0    10     1  343.  1.01 0.987  342.
#> # ℹ 545,480 more rows
#> 
#> $cohpv
#> # A tibble: 1,044 × 3
#>        j tzero    spv
#>    <int> <dbl>  <dbl>
#>  1     1     0 38831.
#>  2     2     0 38760.
#>  3     3     0 38689.
#>  4     4     0 38617.
#>  5     5     0 38545.
#>  6     6     0 38473.
#>  7     7     0 38401.
#>  8     8     0 38328.
#>  9     9     0 38255.
#> 10    10     0 38182.
#> # ℹ 1,034 more rows
#> 
#> $total
#> [1] 16770896
#> 
#> $mean
#> [1] 16064.08
```

We also wish to calculate discounted present values into the future, say
every annually for 10 years. We need a price index that lasts 30 years
(20 year time horizon + up to 10 years of future evaluations). We would
expect this to change over time due to the price index.

``` r
# Present value at time 1, 53, 105, ...
pv2 <- futurepv(
  l = (1:10)*52 - 51,
  payoffs = payoffs$cost_oth,
  prices = prices,
  disc = disc_pt
)
pv2$results
#> $ncoh
#> [1] 1
#> 
#> $uptake
#> [1] 1
#> 
#> $calc
#> # A tibble: 10,440 × 9
#>        j     k     l     t    uj    pk     R     v    pv
#>    <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     1     1     0     1     1  355. 1.00      1  355.
#>  2     1     1    52     1     1  355. 1.05      1  373.
#>  3     1     1   104     1     1  355. 1.10      1  392.
#>  4     1     1   156     1     1  355. 0.579     1  206.
#>  5     1     1   208     1     1  355. 0.602     1  214.
#>  6     1     1   260     1     1  355. 0.627     1  222.
#>  7     1     1   312     1     1  355. 0.652     1  231.
#>  8     1     1   364     1     1  355. 0.678     1  240.
#>  9     1     1   416     1     1  355. 0.705     1  250.
#> 10     1     1   468     1     1  355. 0.733     1  260.
#> # ℹ 10,430 more rows
#> 
#> $cohpv
#> # A tibble: 10 × 3
#>        j tzero    spv
#>    <int> <dbl>  <dbl>
#>  1     1     0 38831.
#>  2     1    52 37503.
#>  3     1   104 34075.
#>  4     1   156 26691.
#>  5     1   208 27759.
#>  6     1   260 28869.
#>  7     1   312 30024.
#>  8     1   364 31225.
#>  9     1   416 32474.
#> 10     1   468 33773.
#> 
#> $total
#> # A tibble: 10 × 2
#>    tzero  total
#>    <dbl>  <dbl>
#>  1     0 38831.
#>  2    52 37503.
#>  3   104 34075.
#>  4   156 26691.
#>  5   208 27759.
#>  6   260 28869.
#>  7   312 30024.
#>  8   364 31225.
#>  9   416 32474.
#> 10   468 33773.
#> 
#> $mean
#> # A tibble: 10 × 2
#>    tzero   mean
#>    <dbl>  <dbl>
#>  1     0 38831.
#>  2    52 37503.
#>  3   104 34075.
#>  4   156 26691.
#>  5   208 27759.
#>  6   260 28869.
#>  7   312 30024.
#>  8   364 31225.
#>  9   416 32474.
#> 10   468 33773.
```
