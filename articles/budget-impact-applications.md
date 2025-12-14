# Budget Impact Applications

## Set-up

First we load the packages necessary for this vignette.

``` r
library(dplyr)
library(lubridate)
library(heemod)
library(dynamicpv)
```

Then let us suppose we have the same cost-effectiveness model and
variables as set-up in
[`vignette("cost-effectiveness-applications")`](https://MSDLLCpapers.github.io/dynacem/articles/cost-effectiveness-applications.md).

## Methods

The package allows us the ability to derive budget impact model
calculations consistent with the cost-effectiveness model shown
previously.

To recap, we had the following assumptions concerning pricing, with a
date of calculation of 2025-09-01.

- Costs are assumed to increases in line with general inflation (2.5%
  per year), except for effects on drug acquisition costs due to LoEs.
- The LoE for the SoC is assumed to occur first, at 2028-01-01, after
  which there is anticipated to be a 70% reduction in prices over one
  year.
- The new intervention has an LoE occuring three years later, at
  2031-01-01, after which there would be a 50% reduction in prices over
  one year.

We had the following assumptions concerning patient uptake.

- Only newly incident patients with the cancer being modeled would be
  eligible for the new treatment. Existing/prevalent patients with the
  condition would not be eligible.
- The disease incidence is 1 patient per week.
- Among these patients, were the new intervention to be made available,
  uptake of the new intervention would be expected to rise linearly from
  0% to 100% after 2 years.

Budget impact models conventionally have no discounting and a shorter
time horizon than cost-effectiveness models, so we will use a time
horizon of 5 years here and a discount rate of 0%. We will compute a
budget impact model using current pricing (per convention) as well as by
using dynamic pricing according to the assumptions previously set.

``` r
# BIM settings
bi_horizon_yrs <- 5
bi_horizon_wks <- round(bi_horizon_yrs / cycle_years)
bi_discount <- 0

# Newly eligible patients
newly_eligible <- rep(1, Ncycles)

# Time for uptake to occur
uptake_years <- 2
uptake_weeks <- round(uptake_years / cycle_years)

# Market share of new intervention
share_multi <- c((1:uptake_weeks)/uptake_weeks, rep(1, Ncycles-uptake_weeks))

# Newly eligible patients receiving each intervention, "world with"
uptake_new <- newly_eligible * share_multi
uptake_soc <- newly_eligible - uptake_new
```

## Results

### Static prices

First we consider static prices, i.e. we assume the prices of existing
resources remain unchanged from now in the horizon of the budget impact
model. Let us use that function to calculate budgetary costs for the
world without the new intervention.

``` r
# World without new intervention

# SoC, drug acquisition costs
wout1_soc_daqcost <- dynpv(
    uptakes = newly_eligible,
    payoffs = hemout_soc$cost_daq_soc_rup,
    horizon = bi_horizon_wks,
    prices = prices_static,
    discrate = bi_discount
    )

# SoC, other costs
wout1_soc_othcost <- dynpv(
    uptakes = newly_eligible,
    payoffs = hemout_soc$cost_nondaq_rup,
    horizon = bi_horizon_wks,
    prices = prices_static,
    discrate = bi_discount
    )

# Total budgetary costs
budget_wout1 <- budget_wout1_soc <- wout1_soc_daqcost + wout1_soc_othcost
```

The total budgetary costs in the world without are \$12,923,366 in
respect of 522 patients.

Let us now calculate the budgetary costs in the world with the new
intervention.

``` r
# World with

# SoC, drug acquisition costs
with1_soc_daqcost <- dynpv(
    uptakes = uptake_soc,
    payoffs = hemout_soc$cost_daq_soc_rup,
    horizon = bi_horizon_wks,
    prices = prices_static,
    discrate = bi_discount
    )

# SoC, other costs
with1_soc_othcost <- dynpv(
    uptakes = uptake_soc,
    payoffs = hemout_soc$cost_nondaq_rup,
    horizon = bi_horizon_wks,
    prices = prices_static,
    discrate = bi_discount
    )

# New intervention, drug acquisition costs
with1_new_daqcost <- dynpv(
    uptakes = uptake_new,
    payoffs = hemout_new$cost_daq_new_rup,
    horizon = bi_horizon_wks,
    prices = prices_static,
    discrate = bi_discount
    )

# New intervention, other costs
with1_new_othcost <- dynpv(
    uptakes = uptake_new,
    payoffs = hemout_new$cost_nondaq_rup,
    horizon = bi_horizon_wks,
    prices = prices_static,
    discrate = bi_discount
    )

# Total
budget_with1_soc <- with1_soc_daqcost + with1_soc_othcost
budget_with1_new <- with1_new_daqcost + with1_new_othcost
budget_with1 <- budget_with1_soc + budget_with1_new
#> Warning in addprod(e1, e2, mult = 1): Uptake vectors differ in length

# Budget impact
bi1_soc <- budget_with1_soc - budget_wout1_soc
#> Warning in addprod(e1, e2, mult = -1): Uptake vectors differ in length
bi1_new <- budget_with1_new
bi1 <- budget_with1 - budget_wout1
```

The budgetary costs in the world with the new intervention are
\$26,964,789, comprising \$3,508,178 in respect of the costs of 103
patients being treated with the SoC, and \$23,456,610 in respect of the
costs of 419 patients being treated with the SoC. The total budget
impact is \$14,041,423, representing an increase of 109%.

### Dynamic prices

Now let us recalculate the budget impact, assuming dynamic pricing in
drug acquisition costs. This is simple with `dynamicpv()::dynpv()`
because we just change the `prices` argument from `prices_static` to
either `prices_dyn_soc` or `prices_dyn_new` for the drug acquisition
costs. We will keep other costs unchanged.

``` r
# World without new intervention

# SoC, drug acquisition costs
wout2_soc_daqcost <- dynpv(
    uptakes = newly_eligible,
    payoffs = hemout_soc$cost_daq_soc_rup,
    horizon = bi_horizon_wks,
    prices = prices_dyn_soc,
    discrate = bi_discount
    )

# SoC, other costs - unchanged from static calculations
wout2_soc_othcost <- wout1_soc_othcost

# Total budgetary costs
budget_wout2 <- budget_wout2_soc <- wout2_soc_daqcost + wout2_soc_othcost
```

The total budgetary costs in the world without are \$11,516,132 in
respect of 522 patients.

Let us now calculate the budgetary costs in the world with the new
intervention.

``` r
# World with

# SoC, drug acquisition costs
with2_soc_daqcost <- dynpv(
    uptakes = uptake_soc,
    payoffs = hemout_soc$cost_daq_soc_rup,
    horizon = bi_horizon_wks,
    prices = prices_dyn_soc,
    discrate = bi_discount
    )

# SoC, other costs
with2_soc_othcost <- with1_soc_othcost

# New intervention, drug acquisition costs
with2_new_daqcost <- dynpv(
    uptakes = uptake_new,
    payoffs = hemout_new$cost_daq_new_rup,
    horizon = bi_horizon_wks,
    prices = prices_dyn_new,
    discrate = bi_discount
    )

# New intervention, other costs
with2_new_othcost <- with1_new_othcost

# Total
budget_with2_soc <- with2_soc_daqcost + with2_soc_othcost
budget_with2_new <- with2_new_daqcost + with2_new_othcost
budget_with2 <- budget_with2_soc + budget_with2_new
#> Warning in addprod(e1, e2, mult = 1): Uptake vectors differ in length

# Budget impact
bi2_soc <- budget_with2_soc - budget_wout2_soc
#> Warning in addprod(e1, e2, mult = -1): Uptake vectors differ in length
bi2_new <- budget_with2_new
bi2 <- budget_with2 - budget_wout2
```

The budgetary costs in the world with the new intervention are
\$28,535,724, comprising \$3,460,107 in respect of the costs of 103
patients being treated with the SoC, and \$25,075,617 in respect of the
costs of 419 patients being treated with the new treatment. The total
budget impact is \$17,019,592, representing an increase of 148%.

### Summary

|                                |                     | Static drug pricing | Dynamic drug pricing |
|:-------------------------------|:--------------------|---------------------|----------------------|
| World without new intervention |                     |                     |                      |
|                                | Standard of Care    | 12,923,366          | 11,516,132           |
|                                | New intervention    | 0                   | 0                    |
|                                | Total               | 12,923,366          | 11,516,132           |
| World with new intervention    |                     |                     |                      |
|                                | Standard of Care    | 3,508,178           | 3,460,107            |
|                                | New intervention    | 23,456,610          | 25,075,617           |
|                                | Total               | 26,964,789          | 25,075,617           |
| Budget impact                  |                     |                     |                      |
|                                | Standard of Care    | -9,415,187          | -8,056,025           |
|                                | New intervention    | 23,456,610          | 25,075,617           |
|                                | Absolute impact     | 14,041,423          | 17,019,592           |
|                                | Relative impact (%) | 109%                | 148%                 |

Budget Impact model results with and without dynamic drug pricing

## Discussion

- In both cases, the budget impact is large, with budgetary costs more
  than doubling with the introduction of the new intervention, over the
  time horizon of interest (5 years).
- Dynamic pricing leads to greater anticipated costs of the new
  intervention and lower expected budgetary costs of the SoC, over the
  time horizon of interest. Accordingly, in this example, the budget
  impact is greater in the dynamic pricing scenario.
- Further stratification of the results, by intervention received, time
  period, cost component etc may reveal further insights. These are
  possible from the results calculated and presented by
  [`dynamicpv::dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
  but not shown in this simple illustration.
