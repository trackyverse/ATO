# Initiate an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md)

This is a wrapper around the usual way of creating S4 objects through
new(). The main purpose of this function is to ensure the ATO is created
with the right table type. See
[`table_type`](https://trackyverse.github.io/ATO/reference/table_type.md)
for more details.

## Usage

``` r
init_ato(det, dep, tag, ani, obs, silent = FALSE)
```

## Arguments

- det:

  an object of class ATO_det. See
  [`make_det`](https://trackyverse.github.io/ATO/reference/make_det.md).

- dep:

  an object of class ATO_dep. See
  [`make_dep`](https://trackyverse.github.io/ATO/reference/make_dep.md).

- tag:

  an object of class ATO_tag. See
  [`make_tag`](https://trackyverse.github.io/ATO/reference/make_tag.md).

- ani:

  an object of class ATO_ani. See
  [`make_ani`](https://trackyverse.github.io/ATO/reference/make_ani.md).

- obs:

  an object of class ATO_obs. See
  [`make_obs`](https://trackyverse.github.io/ATO/reference/make_obs.md).

- silent:

  Supresses summary messages

## Value

an [`ATO`](https://trackyverse.github.io/ATO/reference/ATO.md)
