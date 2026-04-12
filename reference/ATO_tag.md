# S4 class: ATO_tag

An S4 class for the
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) tags (@tag).
Does not contain internal slots. It is a table of either data.frame,
data.table, or tibble format.

## Usage

``` r
.ATO_tag
```

## Format

A data frame with 0 rows and 12 variables:

- manufacturer:

  Manufacturer of the transmitter, character.

- model:

  Model of the transmitter, character.

- power_level:

  Power level of the transmitter, real.

- ping_rate:

  Expected ping rate of the transmitter, numeric. In seconds.

- ping_variation:

  Range of the variation added between pings, to reduce tag collisions,
  numeric. In seconds

- serial:

  Serial number of the tag, character.

- transmitter:

  Transmitter code, character.

- activation_datetime:

  date and time of the tag activation, posixct.

- battery_life:

  expected battery duration of the tag, numeric.

- sensor_type:

  Type of sensor data associated with the transmitter, character.

- sensor_unit:

  Unit of the data associated with the transmitter, character.

- animal:

  Name of the animal that received the tag, character.

An object of class `ATO_tag` (inherits from `data.frame`) with 0 rows
and 13 columns.

## Details

The prototype @tag slot contains the standard columns of the @tag slot.
Other columns are allowed, but these are necessary, and must be of the
designated types.

The tag slot contains information on the different transmitters being
tracked. Tags with multiple transmitter codes are coded as multiple rows
associated with a single serial number and a single animal.

Can be of type data.frame, data.table, or tibble, depending on the @tbl
slot. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details on
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) table types.

## See also

make_tag
