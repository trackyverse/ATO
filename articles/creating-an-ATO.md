# Creating an ATO

## data.frame, data.table, or tibble?

The Animal Tracking Object (ATO) supports the three types of
tables[¹](#fn1), so R users can use the flavour they prefer. The default
table type of an ATO is data.frame. You may change this for all future
ATOs in the current R session by running either
`ato_table_type_global("data.table")` or
`ato_table_type_global("tibble")`. To check the table type of an
already-created ATO, use `table_type(ato)`, and to modify it, run
e.g. `table_type(ato) <- "data.table"`. ATOs also indicate their table
types when you show them in the console.

Packages within the trackyverse may change the internal table formats of
an ATO provided to them, but should always return the ATO in the format
that it was provided in the first place. Packages should *not* use
[`ato_table_type_global()`](https://trackyverse.github.io/ATO/reference/ato_table_type_global.md).
Further, if your package uses data.table ATOs, you should make it clear
to the user whether or not your package is making a copy of the ATO and
editing it, or working directly on the provided object (which might
affect its contents even if the function eventually crashes).

## Creating the ATO object

The correct way to create an ATO is through the function
[`init_ato()`](https://trackyverse.github.io/ATO/reference/init_ato.md).
This will ensure that the created ATO respects the current settings
(e.g. the default table type). The
[`init_ato()`](https://trackyverse.github.io/ATO/reference/init_ato.md)
function can be called without arguments, which will generate an empty
ATO.

``` r
library(ATO)

ato <- init_ato()
ato
#> data.frame ATO object:
#> @det slot is empty.
#> @dep slot is empty.
#> @tag slot is empty.
#> @ani slot is empty.
#> @obs slot is empty.
#> @log slot is empty.
```

When you open an ATO, you’ll find a short summary of its contents. The
output above shows us that our newly created ATO is empty, and also
reveals the six core slots of the ATO. You can find more about those in
the [article about the ATO
structure](https://ato.trackyverse.org/articles/ATO-structure.html). The
family of “make” functions
(e.g. [`make_det()`](https://trackyverse.github.io/ATO/reference/make_det.md))
is responsible for creating each of the slots, which can then be
integrated into your ATO object using
[`add()`](https://trackyverse.github.io/ATO/reference/add.md). You may
also initiate an ATO already with data by using the respective arguments
within
[`init_ato()`](https://trackyverse.github.io/ATO/reference/init_ato.md).
Below is an example with dummy data. Note:
[`add()`](https://trackyverse.github.io/ATO/reference/add.md) has an
`append` argument that you can use to add more data to a slot that
already contains some information.

Note that functions in ATO are designed to be verbose in the choices
they make. This can be annoying, but it is an effort to prevent the ATO
from becoming a “black box”. Some warnings will be shown in the workflow
below.

We’ll start by creating a simple ATO containing information on one
animal:

``` r
ani <- make_ani(
  animal = "A1",
  length = 123,
  weight = 55,
  release_datetime = Sys.time() - 20,
  release_location = "Narnia",
  tz = "America/Halifax"
)

# method 1: use add()
ato <- add(ato, ani)
ato
#> data.frame ATO object:
#> @det slot is empty.
#> @dep slot is empty.
#> @tag slot is empty.
#> @ani:
#>  - 1 animal in total
#>  - Not yet matched to @tag
#>  - Not yet matched to @det (through @tag)
#>  - Not yet matched to @obs
#>  - 1 release location
#> @obs slot is empty.
#> @log:
#>  - 1 log entry from 1 package
```

We can add in some tag information. Note that this information is
automatically matched to the `@ani` slot (the animals) created above.

``` r
tag <- make_tag(
  transmitter = c("R64K-1234", "R64K-1235"),
  animal = c("A1", "A1"),
  tz = "America/Halifax"
)
ato <- add(ato, tag)
#> M: Matching @ani to @tag...
#> M: Matching @ani to @tag took 0.002s
ato
#> data.frame ATO object:
#> @det slot is empty.
#> @dep slot is empty.
#> @tag:
#>  - 2 transmitter codes
#>  - All matched to animals
#>  - Not yet matched to @det
#>  - Not yet matched to @obs
#> @ani:
#>  - 1 animal in total
#>  - All with associated tags
#>  - Not yet matched to @det (through @tag)
#>  - Not yet matched to @obs
#>  - 1 release location
#> @obs slot is empty.
#> @log:
#>  - 2 log entries from 1 package
```

Similarly, let’s add receiver deployments…

``` r
dep <- make_dep(
  receiver_serial = as.character(c(11111, 22222)),
  deploy_location = "Narnia",
  deploy_datetime = Sys.time() - 60,
  deploy_lat = c(11.123, 11.124),
  deploy_lon = c(21.234, 21.235),
  deploy_z = c(20, 30),
  recover_datetime = Sys.time() + 120,
  tz = "America/Halifax"
)

ato <- add(ato, dep)
#> M: Matching @ani to @tag...
#> M: Matching @ani to @tag took 0.001s
```

…and animal detections.

``` r
det <- make_det(
  datetime = c(Sys.time(), Sys.time() + 1),
  receiver_serial = as.character(c(11111, 22222)),
  transmitter = c("R64K-1234", "R64K-1234"),
  tz = "America/Halifax"
)
#> Warning: datetime contains millisecond information. Because frac_second was not
#> provided, fractional seconds will be stripped from the datetime and stored in
#> the frac_second column instead. To avoid this warning, strip fractional seconds
#> from the timestamps and optionally add them through the frac_second argument.
```

Wait a minute, what’s that warning?? Nothing to worry about! ATO found
that we provided sub-second information and broke it out into a column
called `frac_second` for us to protect it from being overwritten if the
`datetime` column is modified.

``` r
det
#>              datetime frac_second receiver_serial transmitter sensor_value
#> 1 2026-04-09 16:44:28   0.5457439           11111   R64K-1234           NA
#> 2 2026-04-09 16:44:29   0.5457473           22222   R64K-1234           NA
#>   valid
#> 1  TRUE
#> 2  TRUE
```

We can accept this warning and move on, or strip out the fractional
second information ourselves and supply it directly.

``` r
times <- c(Sys.time(), Sys.time() + 1)
frac_seconds <- as.numeric(times) %% 1

det <- make_det(
  datetime = times - frac_seconds,
  frac_second = frac_seconds,
  receiver_serial = as.character(c(11111, 22222)),
  transmitter = c("R64K-1234", "R64K-1234"),
  tz = "America/Halifax"
)

ato <- add(ato, det)
#> M: Matching @ani to @tag...
#> M: Matching @ani to @tag took 0.001s
#> M: Matching @det to @tag...
#> Warning in data.table::foverlaps(x@det, x@tag, nomatch = NA, which = TRUE):
#> POSIXct interval cols have mixed timezones. Overlaps are performed on the
#> internal numerical representation of POSIXct objects (always in UTC epoch
#> time), therefore printed values may give the impression that values don't
#> overlap but their internal representations do Please ensure that POSIXct type
#> interval cols have identical 'tzone' attributes to avoid confusion.
#> M: 1 valid tag has no valid detection.
#> M: Matching @det to @tag took 0.059s
#> M: Matching @dep to @det...
#> M: 2 valid transmitter deployments have no valid detections.
#> M: Matching @dep to @det took 0.012s
```

Oh, no! Another warning. This one stems directly from
[`data.table::fread`](https://rdrr.io/pkg/data.table/man/fread.html),
the function being used to join everything together under the hood. In
this instance, the time zone of the `datetime` column is different to
the time zone specified (“America/Halifax”). We are warned that the join
has been conducted in UTC, but might be confusing if you are expecting
times in one of the other time zones on offer. This warning can be
avoided by making sure all time zones are the same!

Here is what the final ATO looks like:

``` r
ato
#> data.frame ATO object:
#> @det:
#>  - 2 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 1 transmitter detected in total
#>  - 2 receivers in total
#>  - Data range: 2026-04-09 16:44:28 to 2026-04-09 16:44:29 (America/Halifax)
#> @dep:
#>  - 2 deployments in total
#>  - No invalid deployments
#>  - 2 receivers deployed
#>  - 1 beacon transmitter deployed
#>  - 1 listed deployment location
#> @tag:
#>  - 2 transmitter codes
#>  - All matched to animals
#>  - 1 never detected - Not yet matched to @obs
#> @ani:
#>  - 1 animal in total
#>  - All with associated tags
#>  - All detected
#>  - Not yet matched to @obs
#>  - 1 release location
#> @obs slot is empty.
#> @log:
#>  - 4 log entries from 1 package
```

## All at once

Rather than build the ATO up sequentially via `add`, we can do it all at
once when initiating the ATO:

``` r
ato <- init_ato(
  ani = ani,
  tag = tag,
  dep = dep,
  det = det
)
#> M: Matching @ani to @tag...
#> M: Matching @ani to @tag took 0.001s
#> M: Matching @det to @tag...
#> Warning in data.table::foverlaps(x@det, x@tag, nomatch = NA, which = TRUE):
#> POSIXct interval cols have mixed timezones. Overlaps are performed on the
#> internal numerical representation of POSIXct objects (always in UTC epoch
#> time), therefore printed values may give the impression that values don't
#> overlap but their internal representations do Please ensure that POSIXct type
#> interval cols have identical 'tzone' attributes to avoid confusion.
#> M: 1 valid tag has no valid detection.
#> M: Matching @det to @tag took 0.012s
#> M: Matching @dep to @det...
#> M: 2 valid transmitter deployments have no valid detections.
#> M: Matching @dep to @det took 0.01s
ato
#> data.frame ATO object:
#> @det:
#>  - 2 detections in total
#>  - No stray detections
#>  - No orphan detections
#>  - No invalid detections
#>  - 1 transmitter detected in total
#>  - 2 receivers in total
#>  - Data range: 2026-04-09 16:44:28 to 2026-04-09 16:44:29 (America/Halifax)
#> @dep:
#>  - 2 deployments in total
#>  - No invalid deployments
#>  - 2 receivers deployed
#>  - 1 beacon transmitter deployed
#>  - 1 listed deployment location
#> @tag:
#>  - 2 transmitter codes
#>  - All matched to animals
#>  - 1 never detected - Not yet matched to @obs
#> @ani:
#>  - 1 animal in total
#>  - All with associated tags
#>  - All detected
#>  - Not yet matched to @obs
#>  - 1 release location
#> @obs slot is empty.
#> @log:
#>  - 4 log entries from 1 package
```

And that’s it! Now you have an ATO you can use in further analyses.

For package developers: [how should I handle an ATO inside my
functions?\[still to be
written\]](https://ato.trackyverse.org/articles/ATO-manipulation.html)

For users: [what can I do with my ATO?\[still to be
written\]](https://ato.trackyverse.org/articles/tracky-packages.html)

------------------------------------------------------------------------

1.  Provided you have the respective packages installed.
