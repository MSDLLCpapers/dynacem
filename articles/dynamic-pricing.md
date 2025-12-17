# Dynamic Pricing

## What is dynamic pricing?

Dynamic pricing is pricing of a resource (e.g. acquisition cost of a
medicine, wages of a professional) that changes in time. Often, we might
assume that prices are constant in real terms. In other words, were it
not for price inflation, prices would be constant. This assumption
provides a lot of mathematical convenience in that we can keep
calculations of Net Present Value (NPV) a lot simpler.

The NPV of a cashflow \\c_t\\ in \\t\\ years time, assuming prices
constant in real terms and an annual discount rate of \\i\_{real}\\ is
calculated as follows:

\\NPV = \frac{c_t}{(1+i\_{real})^{t}}\\

Cashflows may not in general be expected to be constant in real terms.
Accordingly it can be preferable to explicitly estimate the cashflow in
nominal terms, \\c'\_t = c_t \cdot R_t\\, and discount with a nominal
discount rate that includes expected inflation.

\\ 1+i\_{nom} = (1+i\_{real})(1+r_g) \\ \\NPV =
\frac{c'\_t}{(1+i\_{nom})^t} \\

## Using *dynamicpv*

First let us load the packages we will use for this vignette.

``` r
library(dynamicpv)
```

### Cashflows constant in real terms

It is overkill rather to use *dynamicpv* to calculate the NPV of a
cashflow that is constant in real terms, since there are simpler routes
to doing this. Nevertheless, let us start there, since it helps
understand functionality.

``` r
# A simple cashflow
cashflow <- c(110, 120, 130, 140, 150)

# (Real) discount rate of 3\% per timestep (year)
disc <- 0.03
vt1 <- (1 + disc)^(-1 * (0:4))

# Run dynpv calculation with full output
pv1 <- dynpv(payoffs=cashflow, discrate=disc)
pv1
#> # A tibble: 5 × 9
#>       j     k     l     t    uj    pk     R     v    pv
#>   <int> <int> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1     1     1     0     1     1   110     1 1      110 
#> 2     1     2     0     2     1   120     1 0.971  117.
#> 3     1     3     0     3     1   130     1 0.943  123.
#> 4     1     4     0     4     1   140     1 0.915  128.
#> 5     1     5     0     5     1   150     1 0.888  133.

# Total present value
total(pv1)
#> [1] 610.4352
```

The calculation can be presented in a table as follows.

| Time  | Cashflow | Discount factor       | Product |
|-------|----------|-----------------------|---------|
| \\t\\ | \\c_t\\  | \\v^t = (1+i)^{1-t}\\ |         |
| 1     | 110      | 1                     | 110     |
| 2     | 120      | 0.971                 | 117     |
| 3     | 130      | 0.943                 | 123     |
| 4     | 140      | 0.915                 | 128     |
| 5     | 150      | 0.888                 | 133     |
| Total | 650      |                       | 610     |

Calculation of NPV from a simple cashflow and given discount rate

The present value of this cashflow is 610. The same result could have
been found more easily than invoking this package.

``` r
sum(vt1 * cashflow)
#> [1] 610.4352
```

### Cashflow escalating at constant rate

In this setting, let us assume that the cashflow escalates at a constant
rate, but that this constant rate is not equal to the general price
inflation one would include in a nominal discount rate. In this example,
let us assume that the price of the cashflow increases at 1 % per year,
but that general price inflation is 2.5 % per year. The real discount
rate is unchanged at 3 % per year.

First, we derive a price index to handle the changing price of the
resource underlying the cashflow. Then we derive the nominal discount
rate, incorporating the general rate of price inflation in the economy.
We then combine these inputs with
[`dynamicpv::dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
to calculate the NPV.

``` r
# Set up price index
pinfl <- 0.01
pindex <- (1+pinfl)^(0:4)
pindex
#> [1] 1.000000 1.010000 1.020100 1.030301 1.040604

# Nominal discount rate
nomdisc <- (1+disc)*1.025-1

# Calculate present value
pv2 <- dynpv(payoffs=cashflow, prices=pindex, discrate=nomdisc)
total(pv2)
#> [1] 591.9954
```

The NPV of 592 could still have been calculated using base R functions.

``` r
# Compare with more base calculations
vt2 <- (1+nomdisc)^(-1 * (0:4))
sum(vt2 * cashflow * pindex)
#> [1] 591.9954
```

### Cashflow changing irregularly

Where *dynamicpv* is most relevant is when the price index is more
irregular, more dynamic. For example, one might anticipate a sudden
future change in a price of a certain resource (e.g. drug price
reduction on loss of exclusivity). This can be accommodated in the price
index. In this simple example, let us assume that the underlying price
of the resource reduces to half its original value from the fourth year.

``` r
# Revise the price index to be 0.5 from year 4
pindex[4:5] <- 0.5
pindex
#> [1] 1.0000 1.0100 1.0201 0.5000 0.5000

# Calculate present value
pv3 <- dynpv(payoffs=cashflow, prices=pindex, discrate=nomdisc)
total(pv3)
#> [1] 463.6326
```

With this dynamic pricing change, the NPV has reduced by 0, from 464 to
464. The corresponding base R code is shown below.

``` r
# Compare with more base calculations
sum(vt2 * cashflow * pindex)
#> [1] 463.6326
```

## Summary

- Dynamic pricing is when pricing of a resource changes in time.
  Although the examples in this vignette can be calculated with base R
  functions relatively easily, it is evident how calculations of NPV can
  quickly become complicated.
- The
  [`dynamicpv::dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
  function can be used to calculate NPVs for arbitrary vectors of
  cashflows, at a given discount rate and a given underlying price
  index.
- In this way, the function can accommodate as needed irregular prices
  changes, regular price changes, or no price change at all.
