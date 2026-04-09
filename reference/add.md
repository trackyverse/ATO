# Generic add function

Brings already-formatted data into the ATO. Adequated methods are picked
based on the type of data being incorporated.

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
