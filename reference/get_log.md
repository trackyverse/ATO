# Generic to extract the log slot as a table

Generic to extract the log slot as a table

## Usage

``` r
get_log(x, debug = FALSE)

# S4 method for class 'ATO'
get_log(x, debug = FALSE)
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) object

- debug:

  should debug entries and the call column be displayed?

## Value

The log slot as a table

## Examples

``` r
# extract all the log lines from an ATO in table format
log <- get_log(example_ato)
head(log)
#>              datetime type pkg fun                                          log
#> 1 2026-04-12 21:42:01  log ATO add Added a new @det slot with 14544 detections.
#> 2 2026-04-12 21:42:02  log ATO add   Added a new @dep slot with 17 deployments.
#> 3 2026-04-12 21:42:02  log ATO add          Added a new @tag slot with 60 tags.
#> 4 2026-04-12 21:42:02  log ATO add       Added a new @ani slot with 60 animals.

# clean up
rm(log)
```
