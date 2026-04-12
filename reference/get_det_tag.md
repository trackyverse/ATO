# A wrapper of [`get_det`](https://trackyverse.github.io/ATO/reference/get_det.md) to extract detections for transmitters listed in the tag slot

A wrapper of
[`get_det`](https://trackyverse.github.io/ATO/reference/get_det.md) to
extract detections for transmitters listed in the tag slot

## Usage

``` r
get_det_tag(x, receivers, type = c("all", "valid", "invalid"))

# S4 method for class 'ATO'
get_det_tag(x, receivers, type = c("all", "valid", "invalid"))
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
# listed in the @tag slot
det <- get_det_tag(example_ato)

# extract only detections from one or more specific receivers
det <- get_det_tag(example_ato, receivers = "132908")
summary(det)
#> @det:
#>  - 708 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 54 transmitters detected in total
#>  - 1 receiver in total
#>  - Data range: 2018-04-14 01:26:37 to 2018-05-02 06:14:42 (Europe/Copenhagen)

# clean up
rm(det)
```
