# Total present value

Sum of the Present Value, by time at which the calculation is performed
(`tzero` input to
[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md))

## Usage

``` r
total(df)
```

## Arguments

- df:

  Tibble of class "dynpv" created by
  [`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md)
  or
  [`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)

## Value

A number or tibble

## Details

The [Present Value](https://en.wikipedia.org/wiki/Present_value) of a
cashflow \\p_k\\ for the \\u_j\\ patients who began treatment at time
\\j\\ and who are in their \\k\\th timestep of treatment is as follows
\$\$PV(j,k,l) = u_j \cdot p_k \cdot R\_{j+k+l-1} \cdot (1+i)^{2-j-k}\$\$
where \\i\\ is the risk-free discount rate per timestep, \\p_k\\ is the
cashflow amount in today’s money, and \\p_k \cdot R\_{j+k+l-1}\\ is the
nominal amount of the cashflow at the time it is incurred, allowing for
an offset of \\l = tzero\\.

The total present value by time at which the calculation is performed,
\\TPV(l)\\, is therefore the sum of \\PV(j,k,l)\\ over all \\j\\ and
\\k\\ within the time horizon \\T\\, namely: \$\$TPV(l) =
\sum\_{j=1}^{T} \sum\_{k=1}^{T-j+1} PV(j,k, l) \\ \\ = \sum\_{j=1}^{T}
\sum\_{k=1}^{T-j+1} u_j \cdot p_k \cdot R\_{l+j+k-1} \cdot
(1+i)^{2-j-k}\$\$

## See also

[`dynpv()`](https://MSDLLCpapers.github.io/dynacem/reference/dynpv.md),
[`futurepv()`](https://MSDLLCpapers.github.io/dynacem/reference/futurepv.md)
