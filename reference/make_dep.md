# Make an ATO deployments object

Formats the input data into the ATO format and appends the ATO_dep
class.

## Usage

``` r
make_dep(
  receiver_model = NA_character_,
  receiver_serial,
  receiver_codeset = NA_character_,
  deploy_location,
  deploy_datetime,
  deploy_lat = NA_real_,
  deploy_lon = NA_real_,
  deploy_z = NA_real_,
  recover_datetime,
  recover_lat = NA_real_,
  recover_lon = NA_real_,
  transmitter = NA_character_,
  transmitter_manufacturer = NA_character_,
  transmitter_ping_rate = NA_real_,
  transmitter_model = NA_character_,
  transmitter_serial = NA_character_,
  tz,
  ...
)
```

## Arguments

- receiver_model:

  Model of the receiver, character. Optional.

- receiver_serial:

  Receiver serial number, integer. Mandatory.

- receiver_codeset:

  Codeset of the receiver, character. Optional.

- deploy_location:

  Name of the location where the receiver was deployed, character.
  Mandatory.

- deploy_datetime:

  date and time of the deployment, posixct. Mandatory.

- deploy_lat:

  latitude of the deployment. Preferably in WGS84, numeric. Optional.

- deploy_lon:

  longitude of the deployment. Preferably in WGS84, numeric. Optional.

- deploy_z:

  depth of the deployment, as measured from the reference surface of the
  water body, numeric. Optional.

- recover_datetime:

  date and time of the recovery, posixct. Mandatory.

- recover_lat:

  latitude of the recovery point. Preferably in WGS84, numeric.
  Optional.

- recover_lon:

  longitude of the recovery point. Preferably in WGS84, numeric.
  Optional.

- transmitter:

  Transmitter code for a beacon/reference tag, character. Optional.

- transmitter_manufacturer:

  Manufacturer of the transmitter, character. Optional.

- transmitter_ping_rate:

  Expected ping rate of the transmitter, numeric. In seconds. Required
  if transmitter is provided.

- transmitter_model:

  Model of the transmitter, character. Optional.

- transmitter_serial:

  Serial number of the transmitter, integer. Required if transmitter is
  provided.

- tz:

  the timezone of the datetime data. Mandatory.

- ...:

  Non-standard columns to be added to the table.

## Value

an ATO_dep object, ready to be used by
[`add`](https://trackyverse.github.io/ATO/reference/add.md) or
[`init_ato`](https://trackyverse.github.io/ATO/reference/init_ato.md).

## See also

[`ATO_dep`](https://trackyverse.github.io/ATO/reference/ATO_dep.md)
