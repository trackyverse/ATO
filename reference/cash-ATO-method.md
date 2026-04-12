# `$` Method to extract ATO slots directly

`$` Method to extract ATO slots directly

## Usage

``` r
# S4 method for class 'ATO'
x$name
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) object

- name:

  the name of the desired ATO slot

## Value

The slot as defined in `name`, along with a warning to directly extract
slots in the future.

## See also

[Extract](https://rdrr.io/r/base/Extract.html)

## Examples

``` r
# access a slot using $ instead of @
dep <- example_ato$dep
#> Warning: `example_ato$dep` converted to `example_ato@dep` for convenience, please use `example_ato@dep` in the future.
#>   See `?slot` for more information.
summary(dep)
#> @dep:
#>  - 17 deployments in total
#>  - No invalid deployments
#>  - 17 receivers deployed
#>  - 1 beacon transmitter deployed
#>  - 17 listed deployment locations

# clean up
rm(dep)
```
