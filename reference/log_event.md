# Log a new event

Logs useful information as the analysis progresses.

## Usage

``` r
log_event(
  ato,
  type = c("log", "message", "msg", "warning", "debug", "comment"),
  ...,
  blame
)
```

## Arguments

- ato:

  the ATO object to which to log the entry.

- type:

  One of several event flags:

  - 'log' saves to the log without displaying.

  - 'message' (or 'msg') displays a message on screen.

  - 'warning' displays a warning on screen.

  - 'debug' flags the entry as debug (not shown by default).

  - 'comment' user comment entry.

- ...:

  The text fragments that compose the event message.

- blame:

  Not used currently

## Value

The updated ATO.
