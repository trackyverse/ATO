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

## Examples

``` r
# extract all the detections from an ATO in table format
det <- get_det(example_ato)
summary(det)
#> @det:
#>  - 14544 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 54 transmitters detected in total
#>  - 16 receivers in total
#>  - Data range: 2018-04-14 01:26:37 to 2018-05-25 20:46:13 (Europe/Copenhagen)

# extract only detections from one or more specific receivers
sub_det <- get_det(example_ato, receivers = "132908")
summary(sub_det)
#> @det:
#>  - 708 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 54 transmitters detected in total
#>  - 1 receiver in total
#>  - Data range: 2018-04-14 01:26:37 to 2018-05-02 06:14:42 (Europe/Copenhagen)

# or matching one or more specific transmitters
sub_det <- get_det(example_ato, transmitters = "R64K-4529")
summary(sub_det)
#> @det:
#>  - 44 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 1 transmitter detected in total
#>  - 7 receivers in total
#>  - Data range: 2018-04-14 01:26:37 to 2018-04-17 05:57:17 (Europe/Copenhagen)

# or both!
sub_det <- get_det(example_ato,
                   receivers = "132908",
                   transmitters = "R64K-4529")
summary(sub_det)
#> @det:
#>  - 10 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 1 transmitter detected in total
#>  - 1 receiver in total
#>  - Data range: 2018-04-14 01:26:37 to 2018-04-14 01:34:45 (Europe/Copenhagen)

# clean up
rm(det, sub_det)
```
