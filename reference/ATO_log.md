# S4 class: ATO_log

An S4 class for the
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.md) log
(@log). Does not contain internal slots. It is a table of either
data.frame, data.table, or tibble format.

## Usage

``` r
.ATO_log
```

## Format

A data frame with 0 rows and 4 variables:

- datetime:

  date and time of the log entry, posixct format.

- package:

  name of the package that hosts the function that made the log entry,
  character.

- call:

  function call that made the log entry, character.

- log:

  log entry, character.

An object of class `ATO_log` (inherits from `data.frame`) with 0 rows
and 6 columns.

## Details

The prototype @log slot contains the standard columns of the @log slot.
Other columns are not allowed.

Contains summary information of the actions performed throughout the
life of the ATO.

Can be of type data.frame, data.table, or tibble, depending on the @tbl
slot. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details on
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.md)
table types.
