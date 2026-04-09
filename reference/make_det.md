# Make an ATO detections object

Formats the input data into the ATO format and appends the ATO_det
class.

## Usage

``` r
make_det(
  datetime,
  frac_second = NA_real_,
  receiver_serial,
  transmitter,
  sensor_value = NA_real_,
  tz,
  ...
)
```

## Arguments

- datetime:

  date and time, posixct format. Mandatory.

- frac_second:

  fractional second, numeric. Optional.

- receiver_serial:

  Mandatory. receiver serial number, integer. Mandatory.

- transmitter:

  Mandatory. transmitter code, character. Mandatory.

- sensor_value:

  reported sensor value, numeric. Optional.

- tz:

  the timezone of the datetime data. Mandatory.

- ...:

  Non-standard columns to be added to the table. Optional.

## Value

an ATO_det object, ready to be used by
[`add`](https://trackyverse.github.io/ATO/reference/add.md) or
[`init_ato`](https://trackyverse.github.io/ATO/reference/init_ato.md).

## See also

ATO_det
