# Present values with dynamic pricing and dynamic uptake

Calculate present value for a payoff with dynamic (lifecycle) pricing
and dynamic uptake (stacked cohorts).

## Usage

``` r
dynpv(
  uptakes = 1,
  payoffs,
  horizon = length(payoffs),
  tzero = 0,
  prices = rep(1, length(payoffs) + tzero),
  discrate = 0
)
```

## Arguments

- uptakes:

  Vector of patient uptake over time

- payoffs:

  Vector of payoffs of interest (numeric vector)

- horizon:

  Time horizon for the calculation (length must be less than or equal to
  the length of payoffs)

- tzero:

  Time at the date of calculation, to be used in lookup in prices vector

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

## Details

Suppose payoffs in relation to patients receiving treatment (such as
costs or health outcomes) occur over timesteps \\t=1, ..., T\\. Let us
partition time as follows.

- Suppose \\j=1,...,T\\ indexes the time at which the patient begins
  treatment.

- Suppose \\k=1,...,T\\ indexes time since initiating treatment.

In general, \\t=j+k-1\\, and we are interested in the set of \\(j,k)\\
such that \\1 \leq t \leq T\\.

For example, \\t=3\\ comprises:

- patients who are in the third timestep of treatment that began in
  timestep 1: (j,k)=(1,3);

- patients who are in the second timestep of treatment that began in
  timestep 2, (j,k)=(2,2); and

- patients who are in the first timestep of treatment that began in
  timestep 3, (j,k)=(3,1)

The [Present Value](https://en.wikipedia.org/wiki/Present_value) of a
cashflow \\p_k\\ for the \\u_j\\ patients who began treatment at time
\\j\\ and who are in their \\k\\th timestep of treatment is as follows
\$\$PV(j,k,l) = u_j \cdot p_k \cdot R\_{j+k+l-1} \cdot (1+i)^{2-j-k}\$\$
where \\i\\ is the risk-free discount rate per timestep, \\p_k\\ is the
cashflow amount in today’s money, and \\p_k \cdot R\_{j+k+l-1}\\ is the
nominal amount of the cashflow at the time it is incurred, allowing for
an offset of \\l = tzero\\.

The total present value, \\TPV(l)\\, is therefore the sum over all \\j\\
and \\k\\ within the time horizon \\T\\, namely: \$\$TPV(l) =
\sum\_{j=1}^{T} \sum\_{k=1}^{T-j+1} PV(j,k, l) \\ \\ = \sum\_{j=1}^{T}
\sum\_{k=1}^{T-j+1} u_j \cdot p_k \cdot R\_{l+j+k-1} \cdot
(1+i)^{2-j-k}\$\$

This function calculates \\PV(j,k,l)\\ for all values of \\j\\, \\k\\
and \\l\\, and returns this in a tibble.

## Examples

``` r
# Simple example
pv1 <- dynpv(
   uptakes = (1:10), # 1 patient uptakes in timestep 1, 2 patients in timestep 2, etc
   tzero = c(0,5), # Calculations are performed with prices at times 0 and 5
   payoffs = 90 + 10*(1:10), # Payoff vector of length 10 = (100, 110, ..., 190)
   prices = 1.02^((1:15)-1), # Prices increase at 2\% per timestep in future
   discrate = 0.05 # The nominal discount rate is 5\% per timestep;
                   # the real discount rate per timestep is 3\% (=5\% - 3\%)
)
pv1
#> # A tibble: 110 × 9
#>        j     k     l     t    uj    pk     R     v    pv
#>    <int> <int> <dbl> <dbl> <int> <dbl> <dbl> <dbl> <dbl>
#>  1     1     1     0     1     1   100  1    1      100 
#>  2     1     1     5     1     1   100  1.10 1      110.
#>  3     1     2     0     2     1   110  1.02 0.952  107.
#>  4     1     2     5     2     1   110  1.13 0.952  118.
#>  5     1     3     0     3     1   120  1.04 0.907  113.
#>  6     1     3     5     3     1   120  1.15 0.907  125.
#>  7     1     4     0     4     1   130  1.06 0.864  119.
#>  8     1     4     5     4     1   130  1.17 0.864  132.
#>  9     1     5     0     5     1   140  1.08 0.823  125.
#> 10     1     5     5     5     1   140  1.20 0.823  138.
#> # ℹ 100 more rows
summary(pv1)
#> Summary of Dynamic Pricing and Uptake
#>      Number of cohorts:             10 
#>      Number of times:               2 
#>      Total uptake:                  55 
#> 
#>  Total and mean present values by timepoint: 
#> # A tibble: 2 × 3
#>   tzero  total  mean
#>   <dbl>  <dbl> <dbl>
#> 1     0 22123.  402.
#> 2     5 24426.  444.

# More complex example, using cashflow output from a heemod model

# Obtain dataset
democe <- get_dynfields(
   heemodel = oncpsm,
   payoffs = c("cost_daq_new", "cost_total", "qaly"),
   discount = "disc"
   )

# Obtain short payoff vector of interest
payoffs <- democe |>
   dplyr::filter(int=="new", model_time<11) |>
   dplyr::mutate(cost_oth = cost_total - cost_daq_new)

# Example calculation
pv2 <- dynpv(
   uptakes = rep(1, nrow(payoffs)),
   payoffs = payoffs$cost_oth,
   prices = 1.05^(0:(nrow(payoffs)-1)),
   discrate = 0.08
)
summary(pv2)
#> Summary of Dynamic Pricing and Uptake
#>      Number of cohorts:             10 
#>      Number of times:               1 
#>      Total uptake:                  10 
#>      Total present value:           16404.32 
#>      Mean present value:            1640.432 
```
