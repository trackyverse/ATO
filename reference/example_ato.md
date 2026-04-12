# Example ATO object

The ATO package comes with a small example ATO object to allow for quick
exploration of the object structure

## Usage

``` r
example_ato
```

## Format

An S4 object with 8 slots:

- ani:

  Information about tagged animals. See
  [`make_ani`](https://trackyverse.github.io/ATO/reference/make_ani.md)

- dep:

  Information about deployments. See
  [`make_dep`](https://trackyverse.github.io/ATO/reference/make_dep.md)

- det:

  Information about detections. See
  [`make_det`](https://trackyverse.github.io/ATO/reference/make_det.md)

- tag:

  Information about tags. See
  [`make_tag`](https://trackyverse.github.io/ATO/reference/make_tag.md)

- obs:

  Information about observations. See
  [`make_obs`](https://trackyverse.github.io/ATO/reference/make_obs.md)

- log:

  Log of actions. See
  [`log_event`](https://trackyverse.github.io/ATO/reference/log_event.md)

- tbl:

  ATO table type. See
  [`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)

- pkg:

  Reserved for other packages of the trackyverse

## Source

The data in the example is the same as provided by the package actel,
and was collected by the author
