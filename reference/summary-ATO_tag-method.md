# Summary method for an ATO_tag slot object

Summary method for an ATO_tag slot object

## Usage

``` r
# S4 method for class 'ATO_tag'
summary(object)
```

## Arguments

- object:

  an ATO_tag slot object

## Value

Nothing. Prints a summary.

## Examples

``` r
summary(get_tag(example_ato))
#> @tag:
#>  - 60 transmitter codes
#>  - All matched to animals
#>  - 6 never detected
#>  - Not yet matched to @obs
```
