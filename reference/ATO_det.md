# S4 class: ATO_det

An S4 class for the
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) detections
(@det). Does not contain internal slots. It is a table of either
data.frame, data.table, or tibble format.

## Usage

``` r
.ATO_det
```

## Format

A data frame with 0 rows and 5 variables:

- datetime:

  date and time, posixct format.

- frac_second:

  fractional second, numeric.

- receiver_serial:

  receiver serial number, character.

- transmitter:

  transmitter code, character.

- sensor_value:

  reported sensor value, numeric.

An object of class `ATO_det` (inherits from `data.frame`) with 0 rows
and 6 columns.

## Details

The prototype @det slot contains the standard columns of the @det slot.
Other columns are allowed, but these are necessary, and must be of the
designated types.

Can be of type data.frame, data.table, or tibble, depending on the @tbl
slot. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details on
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) table types.

## See also

make_det
