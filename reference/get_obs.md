# Generic to extract the obs slot as a table

Generic to extract the obs slot as a table

## Usage

``` r
get_obs(x, type = c("all", "valid", "invalid"))

# S4 method for class 'ATO'
get_obs(x, type = c("all", "valid", "invalid"))
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) object

- type:

  the type of rows to return. One of: 'all' - returns both valid and
  invalid rows (default); 'valid' - returns only valid rows; 'invalid' -
  returns only invalid rows

## Value

The obs slot as a table

## Examples

``` r
# extract all the observations from an ATO in table format
obs <- get_obs(example_ato)
summary(obs)
#> @obs:
#>  -0observations
#>  - No terminal observations
# note: The example ato object has no observations, so this
# returns 0 rows.

# clean up
rm(obs)
```
