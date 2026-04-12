# Summary method for an ATO_det slot object

Summary method for an ATO_det slot object

## Usage

``` r
# S4 method for class 'ATO_det'
summary(object)
```

## Arguments

- object:

  an ATO_det slot object

## Value

Nothing. Prints a summary.

## Examples

``` r
summary(get_det(example_ato))
#> @det:
#>  - 14544 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 54 transmitters detected in total
#>  - 16 receivers in total
#>  - Data range: 2018-04-14 01:26:37 to 2018-05-25 20:46:13 (Europe/Copenhagen)
```
