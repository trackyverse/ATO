# Make an ATO observations object

Formats the input data into the ATO format and appends the ATO_obs
class.

## Usage

``` r
make_obs(
  animal = NA_character_,
  transmitter = NA_character_,
  type = NA_character_,
  terminal,
  location,
  datetime,
  lat = NA_real_,
  lon = NA_real_,
  tz,
  ...
)
```

## Arguments

- animal:

  Name of the animal that was observed, character. Each observation must
  have an animal or transmitter code (or both).

- transmitter:

  Transmitter code that was observed, character. Each observation must
  have an animal or transmitter code (or both).

- type:

  Type of observation (e.g. directly seen, manual tracking), character.
  Optional.

- terminal:

  Was the animal permanently captured at the moment of observation?
  logical. Mandatory.

- location:

  Name of the place where the observation occurred, character.
  Mandatory.

- datetime:

  date and time of the observation, posixct format. Mandatory.

- lat:

  latitude of the observation. Preferably in WGS84, numeric. Optional.

- lon:

  longitude of the observation. Preferably in WGS84, numeric. Optional.

- tz:

  the timezone of the datetime data. Mandatory.

- ...:

  Non-standard columns to be added to the table.

## Value

an ATO_obs object, ready to be used by
[`add`](https://trackyverse.github.io/ATO/reference/add.md) or
[`init_ato`](https://trackyverse.github.io/ATO/reference/init_ato.md).

## See also

[`ATO_obs`](https://trackyverse.github.io/ATO/reference/ATO_obs.md)
