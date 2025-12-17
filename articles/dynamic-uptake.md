# Dynamic Uptake

## What is dynamic uptake?

Dynamic uptake refers to the modeling of multiple series of
payoffs/cashflows over time, rather than the modeling of just one series
of payoffs/cashflows at a time. This may arise, for example, in
considering the treatment costs of multiple cohorts of patients
beginning their treatment courses at different times. There are also
analogies to ‘run-off triangles’ used by general insurance actuaries to
model the emergence of insurance claims over time following claim
events.

This package is designed to be able to calculate presente values with
dynamic uptake/multiple cohorts. This can be coupled with considerations
of dynamic pricing, as covered in
[`vignette("dynamic-pricing")`](https://MSDLLCpapers.github.io/dynacem/articles/dynamic-pricing.md).

## Using *dynamicpv*

First let us load the packages we will use for this vignette.

``` r
library(dynamicpv)
```

Let us work with the same cashflow as in
[`vignette("dynamic-pricing")`](https://MSDLLCpapers.github.io/dynacem/articles/dynamic-pricing.md),
but assume all costs/prices are constant in real terms. The discount
rate is 3 % per year, real.

``` r
# A simple cashflow
cashflow <- c(110, 120, 130, 140, 150)

# (Real) discount rate of 3\% per timestep (year)
disc <- 0.03

# Discounting factor - used for base R calculations later
vt1 <- (1 + disc)^(-1 * (0:4))
```

### Non-dynamic uptake

First let us consider non-dynamic uptake, as covered in
[`vignette("dynamic-pricing")`](https://MSDLLCpapers.github.io/dynacem/articles/dynamic-pricing.md).

| Time  | Cashflow 1 | Discount factor       | Product |
|-------|------------|-----------------------|---------|
| \\t\\ | \\c_t\\    | \\v^t = (1+i)^{1-t}\\ |         |
| 1     | 110        | 1                     | 110     |
| 2     | 120        | 0.971                 | 117     |
| 3     | 130        | 0.943                 | 123     |
| 4     | 140        | 0.915                 | 128     |
| 5     | 150        | 0.888                 | 133     |
| Total | 650        |                       | 610     |

Calculation of NPV from a simple cashflow and given discount rate

In this case, there is a single cashflow series starting at timestep 1.
This time, we will use the `uptakes` argument for
[`dynamicpv::dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md) -
although this is unnecessary since its default is also 1.

``` r
# Uptake vector is simple
uptakes1 <- 1

# NPV calculation
pv1 <- dynpv(payoffs=cashflow, uptakes=uptakes1, discrate=disc)
total(pv1)
#> [1] 610.4352
```

The result is unchanged from the
[`vignette("dynamic-pricing")`](https://MSDLLCpapers.github.io/dynacem/articles/dynamic-pricing.md),
where we also showed the function could be verified with some simple
vector arithmetic.

### Dynamic uptake, constant in time (unweighted)

Suppose we have one new patient each year, and we wish to calculate the
total NPV in a time horizon of 5 years. Now we have a payoff triangle as
follows.

| Time  | Cashflow 1 | Cashflow 2   | Cashflow 3   | Cashflow 4   | Cashflow 5   | Cashflow Sum | Discount factor       | Product |
|-------|------------|--------------|--------------|--------------|--------------|--------------|-----------------------|---------|
| \\t\\ | \\c_t\\    | \\c\_{t-1}\\ | \\c\_{t-2}\\ | \\c\_{t-3}\\ | \\c\_{t-4}\\ |              | \\v^t = (1+i)^{1-t}\\ |         |
| 1     | 110        | \-           | \-           | \-           | \-           | 110          | 1                     | 110     |
| 2     | 120        | 110          | \-           | \-           | \-           | 230          | 0.971                 | 223     |
| 3     | 130        | 120          | 110          | \-           | \-           | 360          | 0.943                 | 339     |
| 4     | 140        | 130          | 120          | 110          | \-           | 500          | 0.915                 | 458     |
| 5     | 150        | 140          | 130          | 120          | 110          | 650          | 0.888                 | 578     |
| Total | 650        |              |              |              |              |              |                       | 1,708   |

Calculation of NPV from a simple cashflow and given discount rate

With
[`dynamicpv::dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md),
we just update the `uptakes` argument.

``` r
# Uptake vector is simple
uptakes2 <- rep(1, 5)
uptakes2
#> [1] 1 1 1 1 1

# NPV calculation
pv2 <- dynpv(payoffs=cashflow, uptakes=uptakes2, discrate=disc)
summary(pv2)
#> Summary of Dynamic Pricing and Uptake
#>      Number of cohorts:             5 
#>      Number of times:               1 
#>      Total uptake:                  5 
#>      Total present value:           1707.723 
#>      Mean present value:            341.5446
```

There are 5 patients with a total NPV of 1,708, which equates to 342 on
average per patient. This can also be calculated in base R.

``` r
sum(vt1 * cumsum(cashflow))
#> [1] 1707.723
```

### Dynamic uptake, not constant in time

In general, uptake will not be constant in time. Due to factors such as
varying epidemiology (prevalence and incidence), prior treatment
pathways (patient testing, patient diagnostics, prior treatment usage),
as well as the specific uptake of the treatment of interest (patient
share), uptake will also be highly variable over time. In this setting,
the total and mean NPVs must reflect weightings applicable to each
cashflow.

For example, suppose the number of patients receiving treatment
increases by one each year. The weighting given to cashflow 1 would then
be \\1/(1+2+3+4+5)=6.67\\ %.

``` r
# Uptake vector is simple
uptakes3 <- 1:5

# NPV calculation
pv3 <- dynpv(payoffs=cashflow, uptakes=uptakes3, discrate=disc)
summary(pv3)
#> Summary of Dynamic Pricing and Uptake
#>      Number of cohorts:             5 
#>      Number of times:               1 
#>      Total uptake:                  15 
#>      Total present value:           3841.785 
#>      Mean present value:            256.119
```

There are 15 patients with a total NPV of 3,842, which equates to 256 on
average per patient. Verifying this result in base R also becomes more
complicated.

``` r
# Verifying total NPV is now more complicated
checkpv <- rep(0, 5)
for (i in 1:5) {
  checkpv[i] <- sum(cashflow[1:i] * uptakes3[i:1] * vt1[i])
}
sum(checkpv)
#> [1] 3841.785
```

### Dynamic pricing and uptake

The package becomes most powerful when considering both dynamic uptake
and dynamic pricing. A simple call to
[`dynamicpv::dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
replaces what would be rather more complicated with base functions, even
in this toy example.

``` r
# Price index
pinfl <- 0.01
pindex <- (1+pinfl)^(0:4)
pindex[4:5] <- 0.5

# Nominal discount rate
nomdisc <- (1+disc)*(1+pinfl)-1

# NPV calculation
pv4 <- dynpv(payoffs=cashflow, uptakes=uptakes3, prices=pindex, discrate=nomdisc)
summary(pv4)
#> Summary of Dynamic Pricing and Uptake
#>      Number of cohorts:             5 
#>      Number of times:               1 
#>      Total uptake:                  15 
#>      Total present value:           2422.633 
#>      Mean present value:            161.5089
```

There are 15 patients with a total NPV of 2,423, which equates to 162 on
average per patient.

## Summary

- Dynamic uptake occurs if the same cashflow series may begin at several
  different times, as can occur when modeling costs of treatment to
  populations over time (where uptake of that treatment varies in time)
  or the run-off of insurance claims. This can complicate calculations
  of NPV.
- The
  [`dynamicpv::dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
  function can be used to calculate NPVs for arbitrary vectors of
  cashflows, at a given discount rate and a given expected uptake over
  time.
- In this way, the function can accommodate both dynamic uptake and
  dynamic pricing, as seen in the
  [`vignette("dynamic-pricing")`](https://MSDLLCpapers.github.io/dynacem/articles/dynamic-pricing.md).
