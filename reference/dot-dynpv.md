# Method to subtract one dynpv object from another

Subtract one object of S7 class "dynpv" from another

## Usage

``` r
# S3 method for class 'dynpv'
e1 - e2
```

## Arguments

- e1:

  First "dynpv" object

- e2:

  Second "dynpv" object

## Value

S3 object of class "dynpv"

## Details

Present value of `e1-e2` is the present values from `e1` less that from
`e2`. Total uptake of `e1-e2` is the uptake from `e1` less that from
`e2`. Take care of this when using `$mean` of the summed object.
