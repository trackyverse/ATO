# Match transmitters in the detections to beacon/reference transmitters in the deployments

Much faster than
[`.match_dep_det_base`](https://trackyverse.github.io/ATO/reference/dot-match_dep_det_base.md)

## Usage

``` r
.match_dep_det_datatable(x, silent)
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md)

- silent:

  Supresses summary messages

## Value

the updated ATO

## Details

Automatically called by the
[`match_update`](https://trackyverse.github.io/ATO/reference/match_update.md)
function.
