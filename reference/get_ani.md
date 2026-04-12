# Generic to extract the ani slot as a table

Generic to extract the ani slot as a table

## Usage

``` r
get_ani(x, type = c("all", "valid", "invalid"))

# S4 method for class 'ATO'
get_ani(x, type = c("all", "valid", "invalid"))
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) object

- type:

  the type of rows to return. One of: 'all' - returns both valid and
  invalid rows (default); 'valid' - returns only valid rows; 'invalid' -
  returns only invalid rows

## Value

The ani slot as a table

## Examples

``` r
# extract all the animals from an ATO in table format
ani <- get_ani(example_ato)
summary(ani)
#> @ani:
#>  - 60 animals in total
#>  - All with associated tags
#>  - 6 with no detections
#>  - Not yet matched to @obs
#>  - 1 release location

# clean up
rm(ani)
```
