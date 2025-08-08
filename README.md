
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dynacem: Evaluate present values and cost-effectiveness with dynamic pricing and uptake <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/MSDLLCpapers/dynacem/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/MSDLLCpapers/dynacem/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/MSDLLCpapers/dynacem/graph/badge.svg)](https://app.codecov.io/gh/MSDLLCpapers/dynacem)
<!-- badges: end -->

The goal of *dynacem* is to evaluate present values and
cost-effectiveness with dynamic pricing and uptake.

Through the *dynpv()* function, the package provides for the present
value of costs, life years, QALYs or other payoffs in the
cost-effectiveness model to be recalculated so as to allow for dynamic
uptake (also known as multiple cohorts) and dynamic (also known as
life-cycle) pricing. The starting point is a conventional cohort
cost-effectiveness model, such as one computed using the
[heemod](https://cran.r-project.org/package=heemod) package.

## Installation

You can install the development version of *dynacem* from
[GitHub](https://github.com/) as follows. You should then load the
package, alongside some other packages used here.

``` r
# Install pak package if not already installed
# install.packages("pak")

# Install dynacem using pak
pak::pak("MSDLLCpapers/dynacem")
#> ! Using bundled GitHub PAT. Please add your own PAT using `gitcreds::gitcreds_set()`.
#> 
#> ✔ Updated metadata database: 962.07 kB in 1 file.
#> 
#> ℹ Updating metadata database
#> ✔ Updating metadata database ... done
#> 
#> 
#> → Will install 30 packages.
#> → Will update 1 package.
#> → Will download 30 CRAN packages (21.43 MB), cached: 1 (0 B).
#> + bit                      4.6.0      [bld][cmp][dl] (304.58 kB)
#> + bit64                    4.6.0-1    [bld][cmp][dl] (142.04 kB)
#> + cli                      3.6.5      [bld][cmp][dl] (640.24 kB)
#> + clipr                    0.8.0      [bld][dl] (21.90 kB) + ✔ libX11-devel
#> + cpp11                    0.5.2      [bld][dl] (291.99 kB)
#> + crayon                   1.5.3      [bld][dl] (40.40 kB)
#> + dplyr                    1.1.4      [bld][cmp][dl] (1.21 MB)
#> + dynacem     0.1.0.9000 → 0.1.0.9000 [bld][cmp] (GitHub: 1098097)
#> + generics                 0.1.4      [bld][dl] (47.22 kB)
#> + glue                     1.8.0      [bld][cmp][dl] (126.68 kB)
#> + hms                      1.1.3      [bld][dl] (43.38 kB)
#> + lifecycle                1.0.4      [bld][dl] (107.66 kB)
#> + magrittr                 2.0.3      [bld][cmp][dl] (267.07 kB)
#> + pillar                   1.11.0     [bld][dl] (409.32 kB)
#> + pkgconfig                2.0.3      [bld][dl] (6.08 kB)
#> + prettyunits              1.2.0      [bld][dl] (97.50 kB)
#> + progress                 1.2.3      [bld][dl] (30.50 kB)
#> + purrr                    1.1.0      [bld][cmp][dl] (263.98 kB)
#> + R6                       2.6.1      [bld][dl] (64.51 kB)
#> + readr                    2.1.5      [bld][cmp][dl] (298.06 kB)
#> + rlang                    1.1.6      [bld][cmp][dl] (767.93 kB)
#> + stringi                  1.8.7      [bld][cmp][dl] (11.91 MB) + ✔ libicu-devel
#> + stringr                  1.5.1      [bld][dl] (176.60 kB)
#> + tibble                   3.3.0      [bld][cmp][dl] (588.67 kB)
#> + tidyr                    1.3.1      [bld][cmp][dl] (809.06 kB)
#> + tidyselect               1.2.1      [bld][cmp][dl] (103.59 kB)
#> + tzdb                     0.5.0      [bld][cmp][dl] (600.72 kB)
#> + utf8                     1.2.6      [bld][cmp][dl] (243.86 kB)
#> + vctrs                    0.6.5      [bld][cmp][dl] (969.07 kB)
#> + vroom                    1.6.5      [bld][cmp][dl] (750.42 kB)
#> + withr                    3.0.2      [bld][dl] (103.24 kB)
#> ✔ All system requirements are already installed.
#> 
#> ℹ Getting 30 pkgs (21.43 MB), 1 cached
#> ✔ Cached copy of bit 4.6.0 (source) is the latest build
#> ✔ Cached copy of bit64 4.6.0-1 (source) is the latest build
#> ✔ Cached copy of cli 3.6.5 (source) is the latest build
#> ✔ Cached copy of clipr 0.8.0 (source) is the latest build
#> ✔ Cached copy of cpp11 0.5.2 (source) is the latest build
#> ✔ Cached copy of generics 0.1.4 (source) is the latest build
#> ✔ Cached copy of hms 1.1.3 (source) is the latest build
#> ✔ Cached copy of lifecycle 1.0.4 (source) is the latest build
#> ✔ Cached copy of pkgconfig 2.0.3 (source) is the latest build
#> ✔ Cached copy of tidyselect 1.2.1 (source) is the latest build
#> ✔ Cached copy of tzdb 0.5.0 (source) is the latest build
#> ✔ Cached copy of withr 3.0.2 (source) is the latest build
#> ✔ Got glue 1.8.0 (source) (126.54 kB)
#> ✔ Got crayon 1.5.3 (source) (40.50 kB)
#> ✔ Got progress 1.2.3 (source) (30.79 kB)
#> ✔ Got readr 2.1.5 (source) (295.60 kB)
#> ✔ Got stringr 1.5.1 (source) (177.06 kB)
#> ✔ Got dplyr 1.1.4 (source) (1.21 MB)
#> ✔ Got purrr 1.1.0 (source) (263.51 kB)
#> ✔ Got utf8 1.2.6 (source) (243.47 kB)
#> ✔ Got R6 2.6.1 (source) (64.41 kB)
#> ✔ Got prettyunits 1.2.0 (source) (98.28 kB)
#> ✔ Got pillar 1.11.0 (source) (408.93 kB)
#> ✔ Got vroom 1.6.5 (source) (746.59 kB)
#> ✔ Got magrittr 2.0.3 (source) (267.20 kB)
#> ✔ Got tidyr 1.3.1 (source) (804.97 kB)
#> ✔ Got tibble 3.3.0 (source) (585.06 kB)
#> ✔ Got rlang 1.1.6 (source) (765.49 kB)
#> ✔ Got vctrs 0.6.5 (source) (967.95 kB)
#> ✔ Got stringi 1.8.7 (source) (11.91 MB)
#> ℹ Building bit 4.6.0
#> ℹ Building cli 3.6.5
#> ℹ Building clipr 0.8.0
#> ℹ Building cpp11 0.5.2
#> ℹ Building crayon 1.5.3
#> ℹ Building generics 0.1.4
#> ℹ Building glue 1.8.0
#> ℹ Building magrittr 2.0.3
#> ℹ Building pkgconfig 2.0.3
#> ℹ Building prettyunits 1.2.0
#> ℹ Building R6 2.6.1
#> ℹ Building rlang 1.1.6
#> ℹ Building stringi 1.8.7
#> ℹ Building utf8 1.2.6
#> ℹ Building withr 3.0.2
#> ✔ Installed dynacem 0.1.0.9000 (github::MSDLLCpapers/dynacem@1098097) (1s)
#> ✔ Built clipr 0.8.0 (1.6s)
#> ✔ Built cpp11 0.5.2 (1.6s)
#> ✔ Built pkgconfig 2.0.3 (1.4s)
#> ✔ Installed clipr 0.8.0  (50ms)
#> ✔ Built R6 2.6.1 (2.5s)
#> ✔ Installed cpp11 0.5.2  (1.1s)
#> ℹ Building tzdb 0.5.0
#> ✔ Built glue 1.8.0 (2.7s)
#> ✔ Built magrittr 2.0.3 (2.7s)
#> ✔ Built prettyunits 1.2.0 (2.7s)
#> ✔ Installed pkgconfig 2.0.3  (1.3s)
#> ✔ Installed glue 1.8.0  (45ms)
#> ✔ Installed magrittr 2.0.3  (84ms)
#> ✔ Installed prettyunits 1.2.0  (121ms)
#> ✔ Installed R6 2.6.1  (66ms)
#> ✔ Built crayon 1.5.3 (3.5s)
#> ✔ Installed crayon 1.5.3  (1s)
#> ✔ Built generics 0.1.4 (4.5s)
#> ✔ Built withr 3.0.2 (4.2s)
#> ✔ Installed generics 0.1.4  (42ms)
#> ✔ Installed withr 3.0.2  (65ms)
#> ✔ Built utf8 1.2.6 (4.9s)
#> ✔ Installed utf8 1.2.6  (1s)
#> ✔ Built bit 4.6.0 (8.2s)
#> ✔ Installed bit 4.6.0  (1s)
#> ℹ Building bit64 4.6.0-1
#> ✔ Built cli 3.6.5 (14.6s)
#> ✔ Installed cli 3.6.5  (1.1s)
#> ✔ Built tzdb 0.5.0 (14.7s)
#> ✔ Built bit64 4.6.0-1 (8.4s)
#> ✔ Installed tzdb 0.5.0  (92ms)
#> ✔ Installed bit64 4.6.0-1  (95ms)
#> ✔ Built rlang 1.1.6 (18.6s)
#> ✔ Installed rlang 1.1.6  (1.1s)
#> ℹ Building lifecycle 1.0.4
#> ✔ Built lifecycle 1.0.4 (1.9s)
#> ✔ Installed lifecycle 1.0.4  (1s)
#> ℹ Building vctrs 0.6.5
#> ✔ Built vctrs 0.6.5 (23.6s)
#> ✔ Installed vctrs 0.6.5  (1.1s)
#> ℹ Building hms 1.1.3
#> ℹ Building pillar 1.11.0
#> ℹ Building purrr 1.1.0
#> ℹ Building tidyselect 1.2.1
#> ✔ Built tidyselect 1.2.1 (2.8s)
#> ✔ Installed tidyselect 1.2.1  (1.1s)
#> ✔ Built pillar 1.11.0 (5.6s)
#> ✔ Installed pillar 1.11.0  (1s)
#> ℹ Building tibble 3.3.0
#> ✔ Built purrr 1.1.0 (6.6s)
#> ✔ Installed purrr 1.1.0  (1.1s)
#> ✔ Built hms 1.1.3 (7.9s)
#> ✔ Installed hms 1.1.3  (1s)
#> ℹ Building progress 1.2.3
#> ✔ Built progress 1.2.3 (2.1s)
#> ✔ Installed progress 1.2.3  (1s)
#> ✔ Built tibble 3.3.0 (5.5s)
#> ✔ Installed tibble 3.3.0  (1.1s)
#> ℹ Building dplyr 1.1.4
#> ℹ Building vroom 1.6.5
#> ✔ Built dplyr 1.1.4 (14.9s)
#> ✔ Installed dplyr 1.1.4  (1.1s)
#> ✔ Built stringi 1.8.7 (1m 25.7s)
#> ✔ Installed stringi 1.8.7  (1.1s)
#> ℹ Building stringr 1.5.1
#> ✔ Built stringr 1.5.1 (5.3s)
#> ✔ Installed stringr 1.5.1  (1.1s)
#> ℹ Building tidyr 1.3.1
#> ✔ Built tidyr 1.3.1 (10.4s)
#> ✔ Installed tidyr 1.3.1  (1.1s)
#> ✔ Built vroom 1.6.5 (1m 23.3s)
#> ✔ Installed vroom 1.6.5  (1.2s)
#> ℹ Building readr 2.1.5
#> ✔ Built readr 2.1.5 (45.8s)
#> ✔ Installed readr 2.1.5  (1.1s)
#> ✔ 1 pkg + 30 deps: upd 1, added 30, dld 18 (19.01 MB) [3m 30.6s]

# Load dynacem and other packages
library(dynacem)
library(ggplot2)
library(tidyr)
```

## Example

Some example present value calculations are provided below for a single
payoff. A typical cost-effectiveness model may involve several separate
payoffs, for each intevention modeled, each with their own price index
and discount rate.

### Obtain payoffs vector

The package comes with a cost-effectiveness model object, *oncpsm*,
created using [heemod](https://cran.r-project.org/package=heemod).

``` r
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
```

The next steps extract a payoff vector from the model object. The
model/object contains several payoffs accumulated in each timestep,
calculated as at time zero:

- drug acquisition cost of the standard of care (*cost_daq_soc*),
- drug acquisition cost of the new intervention (*cost_daq_new*),
- cost of drug administration (*cost_dadmin*),
- cost of disease management (*cost_dman*),
- cost of treating adverse events (*cost_ae*),
- cost of subsequent lines of treatment (*cost_subs*),
- total cost (*cost_total*),
- progression-free life years (*pf_year*),
- life years (*life_year*), and
- QALYs (*qaly*).

The *get_dynfields()* function extracts these parameters from the
[heemod](https://cran.r-project.org/package=heemod) model object, and
calculates ‘rolled-up’ values as at the start of each timestep rather
than discounted to time zero. The rolled-up values are what *dynacem*
requires.

``` r
# Derive the dataset necessary for the dynamic pricing/uptake calculations
democe <- get_dynfields(
    heemodel = oncpsm,
    payoffs = c("cost_daq_new", "cost_total", "qaly"),
    discount = "disc"
    )

# View the extracted data
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

For this example, we are just interested in the payoff for drug
acquisition costs of the new intervention.

``` r
# Obtain a vector of payoffs
payoffs <- democe |>
    dplyr::filter(int=="new") |>
    dplyr::select(model_time, int, cost_daq_new, cost_daq_new_rup)
payoffs
#> # A tibble: 1,044 × 4
#>    model_time int   cost_daq_new cost_daq_new_rup
#>         <int> <chr>        <dbl>            <dbl>
#>  1          1 new          1493.            1493.
#>  2          2 new          1477.            1478.
#>  3          3 new          1461.            1463.
#>  4          4 new          1446.            1448.
#>  5          5 new          1431.            1434.
#>  6          6 new          1416.            1420.
#>  7          7 new          1401.            1406.
#>  8          8 new          1386.            1392.
#>  9          9 new          1372.            1378.
#> 10         10 new          1357.            1364.
#> # ℹ 1,034 more rows
```

### Define dynamic pricing and uptake

Now let us calculate a discounted present value, given dynamic uptake of
one patient per timestep, and dynamic pricing.

- We assume that there are 52 timesteps per year and a discount rate of
  3% (real) per year.
- The general rate of inflation is 5% per year.
- The underlying price of the payoff being costed rises with inflation
  of 5% for the first three years, then drops by 50%, after which it
  rises by 4% per year.

We create a price index twice as long as we need right now for reasons
that should become clear later.

``` r
# Time horizon
Nt <- nrow(payoffs)

# Nominal discount rate
disc_py <- 0.03 + 0.05
disc_pt <- (1+disc_py)^(1/52) - 1

# Price index
prices <- c(1.05^((1:156)/52), 0.5 * (1.05^3) * 1.04^((1:(2 * Nt-156))/52))

# Graphically check the prices index
tibble::tibble(
  years = (1:Nt)/52,
  prices = prices[1:Nt]
  ) |>
  ggplot2::ggplot(aes(x = years, y = prices)) +
    ggplot2::geom_line() +
    ylim(0, 1.5)
```

<img src="man/figures/README-calc1-1.png" width="100%" />

### Calculate current present value

Now with the payoff, uptake, pricing and discount rate set, we can call
the *dynpv()* function and calculate the present value of the payoff.

``` r
# Calculate total discounted present value of drug acquisition costs, given dynamic uptake and pricing
pv1 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_daq_new_rup,
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
#>  1     1     1     0     1     1 1493.  1.00 1     1494.
#>  2     1     2     0     2     1 1478.  1.00 0.999 1478.
#>  3     1     3     0     3     1 1463.  1.00 0.997 1463.
#>  4     1     4     0     4     1 1448.  1.00 0.996 1447.
#>  5     1     5     0     5     1 1434.  1.00 0.994 1432.
#>  6     1     6     0     6     1 1420.  1.01 0.993 1417.
#>  7     1     7     0     7     1 1406.  1.01 0.991 1402.
#>  8     1     8     0     8     1 1392.  1.01 0.990 1388.
#>  9     1     9     0     9     1 1378.  1.01 0.988 1373.
#> 10     1    10     0    10     1 1364.  1.01 0.987 1359.
#> # ℹ 545,480 more rows
#> 
#> $cohpv
#> # A tibble: 1,044 × 3
#>        j tzero     spv
#>    <int> <dbl>   <dbl>
#>  1     1     0 128471.
#>  2     2     0 128253.
#>  3     3     0 128034.
#>  4     4     0 127813.
#>  5     5     0 127592.
#>  6     6     0 127368.
#>  7     7     0 127143.
#>  8     8     0 126917.
#>  9     9     0 126689.
#> 10    10     0 126460.
#> # ℹ 1,034 more rows
#> 
#> $total
#> [1] 54847752
#> 
#> $mean
#> [1] 52536.16
```

### Present values into the future

We also wish to calculate discounted present values (PV) into the
future, say every annually for 10 years.

We need a price index that lasts 30 years (20 year time horizon + up to
10 years of future evaluations). Fortunately our price index is 40 years
long (2 x 20).

We would expect the PV to change over time. The nominal PV will increase
over time due to price inflation of of this payoff of 4% per year. The
real PV will decrease because the rate of price inflation of this
particular payoff (4% per year) is less than the general rate of
inflation (5% per year) factored into the nominal discount rate (8% per
year).

``` r
# Present value at time 0, 52, 104, ...
pv2 <- futurepv(
  tzero = (0:10)*52,
  payoffs = payoffs$cost_daq_new_rup,
  prices = prices,
  disc = disc_pt
)

# Obtain a dataset of the real and nominal present value over time
ds <- pv2$results$mean |>
  dplyr::rename(Nominal = mean) |>
  dplyr::mutate(
    Years = tzero/52,
    pinfl = 1.05^Years,
    Real = Nominal / pinfl
    ) |>
  tidyr::pivot_longer(
    cols = c("Nominal", "Real"),
    names_to = "Type",
    values_to = "PV"
  )

# Plot real and nominal present value over time
ggplot2::ggplot(ds,
  aes(x = Years, y = PV, color=Type)) +
  ggplot2::geom_line() +
  xlim(0, 10) +
  ylim(0, 150000)
```

<img src="man/figures/README-calc2-1.png" width="100%" />
