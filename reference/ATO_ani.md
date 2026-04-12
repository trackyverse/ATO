# S4 class: ATO_ani

An S4 class for the
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) animals
(@ani). Does not contain internal slots. It is a table of either
data.frame, data.table, or tibble format.

## Usage

``` r
.ATO_ani
```

## Format

A data frame with 0 rows and 9 variables:

- animal:

  Name of the animal that received the tag, character.

- capture_location:

  Name of the location where the animal was captured, character.

- capture_datetime:

  date and time of the capture, posixct.

- capture_lat:

  latitude of the capture. Preferably in WGS84, numeric.

- capture_lon:

  longitude of the capture. Preferably in WGS84, numeric.

- release_location:

  Name of the location where the animal was released, character.

- release_datetime:

  date and time of the release, posixct.

- release_lat:

  latitude of the release. Preferably in WGS84, numeric.

- release_lon:

  longitude of the release. Preferably in WGS84, numeric.

An object of class `ATO_ani` (inherits from `data.frame`) with 0 rows
and 10 columns.

## Details

The prototype @ani slot contains the standard columns of the @ani slot.
Other columns are allowed, but these are necessary, and must be of the
designated types.

The ani slot contains information on the animals tagged.

Can be of type data.frame, data.table, or tibble, depending on the @tbl
slot. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details on
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) table types.

## See also

make_ani
