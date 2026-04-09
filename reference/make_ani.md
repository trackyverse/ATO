# Make an ATO animals object

Formats the input data into the ATO format and appends the ATO_ani
class.

## Usage

``` r
make_ani(
  animal,
  capture_location = NA_character_,
  capture_datetime = as.POSIXct(NA_real_),
  capture_lat = NA_real_,
  capture_lon = NA_real_,
  release_location,
  release_datetime,
  release_lat = NA_real_,
  release_lon = NA_real_,
  tz,
  ...
)
```

## Arguments

- animal:

  Name of the animal that received the tag, character. Mandatory.

- capture_location:

  Name of the location where the animal was captured, character.
  Optional.

- capture_datetime:

  date and time of the capture, posixct. Optional.

- capture_lat:

  latitude of the capture. Preferably in WGS84, numeric. Optional.

- capture_lon:

  longitude of the capture. Preferably in WGS84, numeric. Optional.

- release_location:

  Name of the location where the animal was released, character.
  Mandatory.

- release_datetime:

  date and time of the release, posixct. Mandatory.

- release_lat:

  latitude of the release. Preferably in WGS84, numeric. Optional.

- release_lon:

  longitude of the release. Preferably in WGS84, numeric. Optional.

- tz:

  the timezone of the datetime data.

- ...:

  Non-standard columns to be added to the table.

## Value

an ATO_ani object, ready to be used by
[`add`](https://trackyverse.github.io/ATO/reference/add.md) or
[`init_ato`](https://trackyverse.github.io/ATO/reference/init_ato.md).

## See also

ATO_ani
