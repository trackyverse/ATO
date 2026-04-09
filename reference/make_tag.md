# Make an ATO tags object

Formats the input data into the ATO format and appends the ATO_tag
class.

## Usage

``` r
make_tag(
  manufacturer = NA_character_,
  model = NA_character_,
  power_level = NA_real_,
  ping_rate = NA_real_,
  ping_variation = NA_real_,
  serial = NA_character_,
  transmitter,
  activation_datetime = as.POSIXct(NA_real_),
  battery_life = NA_real_,
  sensor_type = NA_character_,
  sensor_unit = NA_character_,
  animal = NA_character_,
  tz,
  ...
)
```

## Arguments

- manufacturer:

  Manufacturer of the transmitter, character. Optional.

- model:

  Model of the transmitter, character. Optional.

- power_level:

  Power level of the transmitter, real. Optional.

- ping_rate:

  Expected ping rate of the transmitter, numeric. In seconds. Optional.

- ping_variation:

  Range of the variation added between pings, to reduce tag collisions,
  numeric. In seconds. Optional.

- serial:

  Serial number of the tag, integer. Optional.

- transmitter:

  Transmitter code, character. Mandatory.

- activation_datetime:

  date and time of the tag activation, posixct. Optional.

- battery_life:

  expected battery duration of the tag, numeric. Optional.

- sensor_type:

  Type of sensor data associated with the transmitter, character.
  Optional.

- sensor_unit:

  Unit of the data associated with the transmitter, character. Optional.

- animal:

  Name of the animal that received the tag, character. Optional.

- tz:

  the timezone of the datetime data. Mandatory.

- ...:

  Non-standard columns to be added to the table.

## Value

an ATO_tag object, ready to be used by
[`add`](https://trackyverse.github.io/ATO/reference/add.md) or
[`init_ato`](https://trackyverse.github.io/ATO/reference/init_ato.md).

## See also

ATO_tag
