# Generic to check if an ATO has data in a given slot

Generic to check if an ATO has data in a given slot

## Usage

``` r
has(object, value, error = FALSE)

# S4 method for class 'ATO'
has(object, value, error = FALSE)
```

## Arguments

- object:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md)

- value:

  a vector with the names of the requested slots

- error:

  Should the code execution stop if the requested slots are empty?

## Value

TRUE if the slots have data, FALSE if they're empty and error = FALSE.
Throws an error if the slots are empty and error = TRUE.

## Examples

``` r
# check if ATO object has detections
has(example_ato, "det")
#> [1] TRUE

# check if ATO has observations
has(example_ato, "obs")
#> [1] FALSE

# alternatively, this would error
# has(example_ato, "obs", TRUE)
```
