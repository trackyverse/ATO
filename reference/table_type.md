# What is the table type of this ATO?

What is the table type of this ATO?

## Usage

``` r
table_type(x, expect)

# S4 method for class 'ATO'
table_type(x, expect)
```

## Arguments

- x:

  an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md)

- expect:

  An expected type of table. Errors if the ATO is not of this type.

## Value

if expect is missing, returns the type of table used by the ATO. If
expect is provided, either errors or returns nothing.
