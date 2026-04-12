# Generic to extract the tag slot as a table

Generic to extract the tag slot as a table

## Usage

``` r
get_tag(x, type = c("all", "valid", "invalid"))

# S4 method for class 'ATO'
get_tag(x, type = c("all", "valid", "invalid"))
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) object

- type:

  the type of rows to return. One of: 'all' - returns both valid and
  invalid rows (default); 'valid' - returns only valid rows; 'invalid' -
  returns only invalid rows

## Value

The tag slot as a table

## Examples

``` r
# extract all the tags from an ATO in table format
tag <- get_tag(example_ato)
summary(tag)
#> @tag:
#>  - 60 transmitter codes
#>  - All matched to animals
#>  - 6 never detected
#>  - Not yet matched to @obs

# clean up
rm(tag)
```
