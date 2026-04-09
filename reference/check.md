# check generic

Checks a prospective ATO slot object to confirm that the contents are as
expected. Matches it with the respective prototype reference.

## Usage

``` r
check(object)

# S4 method for class 'ATO_det'
check(object)

# S4 method for class 'ATO_dep'
check(object)

# S4 method for class 'ATO_tag'
check(object)

# S4 method for class 'ATO_ani'
check(object)

# S4 method for class 'ATO_obs'
check(object)

# S4 method for class 'ATO_tbl'
check(object)
```

## Arguments

- object:

  The prospective ATO slot object

## Value

Nothing. Called to stop() if needed.
