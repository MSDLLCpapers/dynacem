# dynamicpv: Evaluate present values and cost-effectiveness with dynamic pricing and uptake

The goal of *dynamicpv* is to evaluate present values and
cost-effectiveness with dynamic pricing and uptake.

Through the
[dynpv()](https://msdllcpapers.github.io/dynamicpv/reference/dynpv.html)
function, the package provides calculations of the present values of
costs, life years, QALYs or other payoffs allowing for dynamic uptake
(also known as multiple cohorts) and dynamic pricing (also known as
life-cycle). The starting point is a conventional cohort
cost-effectiveness model, such as one computed using the
[heemod](https://cran.r-project.org/package=heemod) package.

## Installation

You can install the development version of *dynamicpv* from
[GitHub](https://github.com/) as follows. (The package was previously
known as *dynacem*, but *dynamicpv* better reflects the package’s
intent.)

``` r
# Install devtools package if not already installed
install.packages("devtools")

# Install dynamicpv using pak from MSD repository
pak::pak("MSDLLCpapers/dynamicpv")
```

Note that the above does not install vignettes. Vignettes may be viewed
on the [package website](https://msdllcpapers.github.io/dynacem) or by
instead running the following.

``` r
devtools::install_github("MSDLLCpapers/dynamicpv", build_vignettes=TRUE)
```

You should then load the package, alongside some other packages used
here.

``` r
# Load the dynamicpv package
library(dynamicpv)

# Recommended others
library(dplyr)
library(heemod)
library(ggplot2)
library(tidyr)
```

## How to use

There are four vignettes provided.

1.  The [Dynamic
    Pricing](https://msdllcpapers.github.io/dynamicpv/articles/dynamic-pricing.html)
    vignette
    ([`vignette("dynamic-pricing")`](https://MSDLLCpapers.github.io/dynacem/articles/dynamic-pricing.md))
    describes how present values may be calculated in the package from a
    payoff vector, allowing for the prices of the resources costed to be
    dynamic.

2.  The [Dynamic
    Uptake](https://msdllcpapers.github.io/dynamicpv/articles/dynamic-uptake.html)
    vignette
    ([`vignette("dynamic-uptake")`](https://MSDLLCpapers.github.io/dynacem/articles/dynamic-uptake.md))
    explores further how present values can be derived allowing for
    dynamic uptake. This is in contrast to the non-dynamic approach of
    calculating a present value for a single cohort of patients. This is
    analogous to the use by general insurance actuaries of run-off
    triangles.

3.  The [Cost-Effectiveness
    Applications](https://msdllcpapers.github.io/dynamicpv/articles/cost-effectiveness-applications.html)
    (`vignette(cost-effectiveness-applications")`) vignette describes
    how, given a static cost-effectiveness model (single cohort, prices
    constant in real terms), Incremental Cost-Effectiveness Ratios may
    be calculated that allow for dynamic pricing and/or dynamic uptake.
    This is illustrated by taking an example three-state partitioned
    survival model of a new intervention for an oncology indication
    compared to standard of care, with stated assumptions for how
    pricing and uptake may be dynamic. Nominal and Real ICERs are
    plotted over time, illustrating the impact of the loss of
    exclusivity points for each intervention.

4.  The [Budget Impact
    Applications](https://msdllcpapers.github.io/dynamicpv/articles/budget-impact-applications.html)
    ([`vignette("budget-impact-applications")`](https://MSDLLCpapers.github.io/dynacem/articles/budget-impact-applications.md))
    vignette describes how, given a static cost-effectiveness model
    (single cohort, prices constant in real terms) and dynamic uptake, a
    budget impact can be readily calculated. The calculations are
    repeated with the dynamic pricing assumptions to illustrate how
    easily this can be done, and examine the differences between a
    budget impact assessment with and without dynamic pricing.
