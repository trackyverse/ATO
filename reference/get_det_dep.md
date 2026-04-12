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

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) object

- receivers:

  An optional vector of receiver serial numbers from which to extract
  detections.

- type:

  the type of rows to return. One of: 'all' - returns both valid and
  invalid rows (default); 'valid' - returns only valid rows; 'invalid' -
  returns only invalid rows

## Value

a table of detections

## Examples

``` r
# wrapper to extract all detections that match transmitters
# listed in the @dep slot (beacon tags)
det <- get_det_dep(example_ato)
summary(det)
#> @det:
#>  - 0 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 0 transmitters detected in total
#>  - 0 receivers in total
# note: The example_ato does not have beacon detections,
# so this returns a table with 0 rows

# extract only detections from one or more specific receivers
det <- get_det_dep(example_ato, receivers = "132908")
summary(det)
#> @det:
#>  - 0 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 0 transmitters detected in total
#>  - 0 receivers in total
# note: The example_ato does not have beacon detections,
# so this returns a table with 0 rows

# clean up
rm(det)
```
