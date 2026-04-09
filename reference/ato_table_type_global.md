# Helper function to set the global type of ATO tables

Helper function to set the global type of ATO tables

## Usage

``` r
ato_table_type_global(
  type = c("data.frame", "data.table", "tibble"),
  update_all = FALSE
)
```

## Arguments

- type:

  the desired type of ATO tables. One of data.frame (default),
  data.table (requires the package data.table installed), or tibble
  (requires the package tibble installed).

- update_all:

  If set to TRUE, this package will search the user's global environment
  for objects of class ATO and will update their table types (by calling
  [`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
  on them).

## Value

Nothing. called for side-effects
