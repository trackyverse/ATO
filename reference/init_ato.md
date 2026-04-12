# Initiate an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md)

This is a wrapper around the usual way of creating S4 objects through
new(). The main purpose of this function is to ensure the ATO is created
with the right table type. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details.

## Usage

``` r
init_ato(det, dep, tag, ani, obs, silent = FALSE)
```

## Arguments

- det:

  an object of class ATO_det. See
  [`make_det`](https://trackyverse.github.io/ATO/reference/make_det.md).

- dep:

  an object of class ATO_dep. See
  [`make_dep`](https://trackyverse.github.io/ATO/reference/make_dep.md).

- tag:

  an object of class ATO_tag. See
  [`make_tag`](https://trackyverse.github.io/ATO/reference/make_tag.md).

- ani:

  an object of class ATO_ani. See
  [`make_ani`](https://trackyverse.github.io/ATO/reference/make_ani.md).

- obs:

  an object of class ATO_obs. See
  [`make_obs`](https://trackyverse.github.io/ATO/reference/make_obs.md).

- silent:

  Supresses summary messages

## Value

an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md)

## Examples

``` r
# split apart the example ATO
ani <- get_ani(example_ato)
dep <- get_dep(example_ato)
det <- get_det(example_ato)
tag <- get_tag(example_ato)

# and now use the parts to build a new ato
x <- init_ato(ani = ani,
              dep = dep,
              det = det,
              tag = tag)
#> M: Matching @ani to @tag...
#> M: Matching @ani to @tag took 0.001s
#> M: Matching @det to @tag...
#> M: 6 valid tags have no valid detection.
#> M: Matching @det to @tag took 0.016s
#> M: Matching @dep to @det...
#> M: 1 valid receiver deployment has no valid detections.
#> M: 17 valid transmitter deployments have no valid detections.
#> M: Matching @dep to @det took 0.017s

# clean up
rm(ani, dep, det, tag, x)
```
