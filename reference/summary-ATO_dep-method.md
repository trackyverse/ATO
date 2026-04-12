# Summary method for an ATO_dep object

Summary method for an ATO_dep object

## Usage

``` r
# S4 method for class 'ATO_dep'
summary(object)
```

## Arguments

- object:

  an ATO_dep object

## Value

Nothing. Prints a summary.

## Examples

``` r
summary(get_dep(example_ato))
#> @dep:
#>  - 17 deployments in total
#>  - No invalid deployments
#>  - 17 receivers deployed
#>  - 1 beacon transmitter deployed
#>  - 17 listed deployment locations
```
