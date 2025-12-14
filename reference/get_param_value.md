# Obtain parameter value(s) from a heemod output

Obtain parameter value(s) from a heemod output

## Usage

``` r
get_param_value(heemodel, param)
```

## Arguments

- heemodel:

  A health economic model object from the *heemod* package (see
  [heemod::heemod-package](https://aphp.github.io/heemod/reference/heemod-package.html)).

- param:

  Name of parameter to extract from the heemod model

## Value

Value of the parameter from the heemod model

## See also

[heemod::heemod-package](https://aphp.github.io/heemod/reference/heemod-package.html)

## Examples

``` r
get_param_value(
   heemodel = oncpsm,
   param = "disc"
   )
#> [1] 0.0005666536
```
