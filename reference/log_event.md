# Log a new event

Logs useful information as the analysis progresses. This function is
meant to be used by other functions within the trackyverse. If you are
looking for a way to add comments to the analysis, look at
[`log_comment`](https://trackyverse.github.io/ATO/reference/log_comment.md).

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

  Not used yet; functionality to come in the future.

## Value

The updated ATO.

## Examples

``` r
# add an event to the ato
x <- log_event(example_ato, type = "msg", "M: just a test")
#> Error in data.frame(datetime = Sys.time(), type = type, pkg = pkg, fun = fun,     call = fun_call, log = event_text): arguments imply differing number of rows: 1, 0
get_log(x)
#> Error in h(simpleError(msg, call)): error in evaluating the argument 'x' in selecting a method for function 'get_log': object 'x' not found
```
