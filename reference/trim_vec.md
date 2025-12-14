# Trim the tailing zeroes from a long vector

Trim the tailing zeroes from a long vector

## Usage

``` r
trim_vec(vec)
```

## Arguments

- vec:

  Vector. Final elements may be zero.

## Value

A vector whose length is shorter than the original, if there were
trailing zero elements

## Examples

``` r
trim_vec(c(1:10, rep(0,3)))
#>  [1]  1  2  3  4  5  6  7  8  9 10
```
