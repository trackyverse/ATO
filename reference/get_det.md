# Generic to extract detections as a table

Generic to extract detections as a table

## Usage

``` r
get_det(x, receivers, transmitters, type = c("all", "valid", "invalid"))

# S4 method for class 'ATO'
get_det(x, receivers, transmitters, type = c("all", "valid", "invalid"))
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) object

- receivers:

  An optional vector of receiver serial numbers from which to extract
  detections.

- transmitters:

  An optional vector of transmitters for which to extract detections.

- type:

  the type of rows to return. One of: 'all' - returns both valid and
  invalid rows (default); 'valid' - returns only valid rows; 'invalid' -
  returns only invalid rows

## Value

a table of detections
