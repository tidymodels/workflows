# Control object for a workflow

`control_workflow()` holds the control parameters for a workflow.

## Usage

``` r
control_workflow(control_parsnip = NULL)
```

## Arguments

- control_parsnip:

  A parsnip control object. If `NULL`, a default control argument is
  constructed from
  [`parsnip::control_parsnip()`](https://parsnip.tidymodels.org/reference/control_parsnip.html).

## Value

A `control_workflow` object for tweaking the workflow fitting process.

## Examples

``` r
control_workflow()
#> <control_workflow>
```
