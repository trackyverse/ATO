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
