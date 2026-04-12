# S4 class: ATO_dep

An S4 class for the
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) deployments
(@dep). Does not contain internal slots. It is a table of either
data.frame, data.table, or tibble format.

## Usage

``` r
.ATO_dep
```

## Format

A data frame with 0 rows and 16 variables:

- receiver_model:

  Model of the receiver, character.

- receiver_serial:

  Receiver serial number, character.

- receiver_codeset:

  Codeset of the receiver, character.

- deploy_location:

  Name of the location where the receiver was deployed, character.

- deploy_datetime:

  date and time of the deployment, posixct.

- deploy_lat:

  latitude of the deployment. Preferably in WGS84, numeric.

- deploy_lon:

  longitude of the deployment. Preferably in WGS84, numeric.

- deploy_z:

  depth of the deployment, as measured from the reference surface of the
  water body, numeric.

- recover_datetime:

  date and time of the recovery, posixct.

- recover_lat:

  latitude of the recovery point. Preferably in WGS84, numeric.

- recover_lon:

  longitude of the recovery point. Preferably in WGS84, numeric.

- transmitter:

  Transmitter code for a beacon/reference tag, character.

- transmitter_manufacturer:

  Manufacturer of the transmitter, character.

- transmitter_ping_rate:

  Expected ping rate of the transmitter, numeric. In seconds.

- transmitter_model:

  Model of the transmitter, character.

- transmitter_serial:

  Serial number of the transmitter, character.

An object of class `ATO_dep` (inherits from `data.frame`) with 0 rows
and 17 columns.

## Details

The prototype @dep slot contains the standard columns of the @dep slot.
Other columns are allowed, but these are necessary, and must be of the
designated types.

Deployments include both the deployments of receivers (where the
receiver information is filled but the transmitter information isn't);
transceivers (where both the receiver and the transmitter information
are filled in; i.e. a receiver with a beacon tag); and reference tags
(where there is no receiver information, but there is transmitter
information).

Can be of type data.frame, data.table, or tibble, depending on the @tbl
slot. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details on
[`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md) table types.

## See also

make_dep
