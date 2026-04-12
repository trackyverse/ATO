# Log a new comment

Logs a user-written comment into the log.

## Usage

``` r
log_comment(ato, ...)
```

## Arguments

- ato:

  the ATO object to which to log the comment

- ...:

  The text fragments that compose the comment.

## Value

the updated ATO.

## Examples

``` r
# add an event to the ato
x <- log_comment(example_ato, "M: just a comment")
get_log(x)
#>              datetime    type pkg         fun
#> 1 2026-04-12 21:42:01     log ATO         add
#> 2 2026-04-12 21:42:02     log ATO         add
#> 3 2026-04-12 21:42:02     log ATO         add
#> 4 2026-04-12 21:42:02     log ATO         add
#> 5 2026-04-12 22:39:14 comment ATO log_comment
#>                                            log
#> 1 Added a new @det slot with 14544 detections.
#> 2   Added a new @dep slot with 17 deployments.
#> 3          Added a new @tag slot with 60 tags.
#> 4       Added a new @ani slot with 60 animals.
#> 5                            M: just a comment
```
