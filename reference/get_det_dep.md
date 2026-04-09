# A wrapper of [`get_det`](https://trackyverse.github.io/ATO/reference/get_det.md) to extract detections for transmitters listed in the dep slot

A wrapper of
[`get_det`](https://trackyverse.github.io/ATO/reference/get_det.md) to
extract detections for transmitters listed in the dep slot

## Usage

``` r
get_det_dep(x, receivers, type = c("all", "valid", "invalid"))

# S4 method for class 'ATO'
get_det_dep(x, receivers, type = c("all", "valid", "invalid"))
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.md)
  object

- receivers:

  An optional vector of receiver serial numbers from which to extract
  detections.

- type:

  the type of rows to return. One of: 'all' - returns both valid and
  invalid rows (default); 'valid' - returns only valid rows; 'invalid' -
  returns only invalid rows

## Value

a table of detections
