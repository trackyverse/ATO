# Generic add function

Brings already-formatted ATO data into an ATO object. Adequated methods
are picked based on the type of data being incorporated.

## Usage

``` r
add(x, value, append = FALSE, silent = FALSE)

# S4 method for class 'ATO,ATO_det'
add(x, value, append = FALSE, silent = FALSE)

# S4 method for class 'ATO,ATO_dep'
add(x, value, append = FALSE, silent = FALSE)

# S4 method for class 'ATO,ATO_tag'
add(x, value, append = FALSE, silent = FALSE)

# S4 method for class 'ATO,ATO_ani'
add(x, value, append = FALSE, silent = FALSE)

# S4 method for class 'ATO,ATO_obs'
add(x, value, append = FALSE, silent = FALSE)
```

## Arguments

- x:

  an ATO

- value:

  The data to be included. The output of one of the make\_\*()
  functions.

- append:

  Logical: Should the new data be appended to the data already present
  in the ATO?

- silent:

  Logical: Supress summary messages

## Value

The updated ATO

## See also

[`make_ani`](https://trackyverse.github.io/ATO/reference/make_ani.md),
[`make_dep`](https://trackyverse.github.io/ATO/reference/make_dep.md),
[`make_det`](https://trackyverse.github.io/ATO/reference/make_det.md),
[`make_tag`](https://trackyverse.github.io/ATO/reference/make_tag.md),
[`make_obs`](https://trackyverse.github.io/ATO/reference/make_obs.md)

## Examples

``` r
# split away parts of the example ATO for the example
ani <- get_ani(example_ato)
dep <- get_dep(example_ato)

# add them to the ATO object using the add methods
x <- add(example_ato, ani)
#> M: Matching @ani to @tag...
#> M: Matching @ani to @tag took 0.002s
#> M: Matching @det to @tag...
#> M: 6 valid tags have no valid detection.
#> M: Matching @det to @tag took 0.073s
#> M: Matching @dep to @det...
#> M: 1 valid receiver deployment has no valid detections.
#> M: 17 valid transmitter deployments have no valid detections.
#> M: Matching @dep to @det took 0.02s
x <- add(x, dep)
#> M: Matching @ani to @tag...
#> M: Matching @ani to @tag took 0.001s
#> M: Matching @det to @tag...
#> M: 6 valid tags have no valid detection.
#> M: Matching @det to @tag took 0.014s
#> M: Matching @dep to @det...
#> M: 1 valid receiver deployment has no valid detections.
#> M: 17 valid transmitter deployments have no valid detections.
#> M: Matching @dep to @det took 0.02s

# clean up
rm(ani, dep, x)
```
