# S4 class: ATO_obs

An S4 class for the
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.md)
observations (@obs). Does not contain internal slots. It is a table of
either data.frame, data.table, or tibble format.

## Usage

``` r
.ATO_obs
```

## Format

A data frame with 0 rows and 8 variables:

- animal:

  Name of the animal that was observed, character.

- transmitter:

  Transmitter code that was observed, character.

- type:

  Type of observation (e.g. directly seen, manual tracking), character.

- terminal:

  Was the animal permanently captured at the moment of observation?
  logical.

- location:

  Name of the place where the observation occurred, character.

- datetime:

  date and time of the observation, posixct format.

- lat:

  latitude of the observation. Preferably in WGS84, numeric.

- lon:

  longitude of the observation. Preferably in WGS84, numeric.

An object of class `ATO_obs` (inherits from `data.frame`) with 0 rows
and 9 columns.

## Details

The prototype @obs slot contains the standard columns of the @obs slot.
Other columns are allowed, but these are necessary, and must be of the
designated types.

The observations slot contains information about locations where an
animal was seen or a tag was heard. E.g. fin observations, or detections
through manual tracking.

Can be of type data.frame, data.table, or tibble, depending on the @tbl
slot. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details on
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO_package.md)
table types.

## See also

make_obs
