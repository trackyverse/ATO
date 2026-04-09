# Wrapper for is(x, "ATO")

This function allows an easy check if the input is an
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.md) and
throw an error if that is not the case.

## Usage

``` r
is_ato(x, error = TRUE)
```

## Arguments

- x:

  the object to test

- error:

  Should the code error if x is not an ATO?

## Value

TRUE if x is an ATO, FALSE if it is not an ATO and error = FALSE. Throws
an error if x is not an ATO and error = TRUE.
