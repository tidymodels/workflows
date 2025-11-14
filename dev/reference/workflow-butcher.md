# Butcher methods for a workflow

These methods allow you to use the butcher package to reduce the size of
a workflow. After calling
[`butcher::butcher()`](https://butcher.tidymodels.org/reference/butcher.html)
on a workflow, the only guarantee is that you will still be able to
[`predict()`](https://rdrr.io/r/stats/predict.html) from that workflow.
Other functions may not work as expected.

## Usage

``` r
axe_call.workflow(x, verbose = FALSE, ...)

axe_ctrl.workflow(x, verbose = FALSE, ...)

axe_data.workflow(x, verbose = FALSE, ...)

axe_env.workflow(x, verbose = FALSE, ...)

axe_fitted.workflow(x, verbose = FALSE, ...)
```

## Arguments

- x:

  A workflow.

- verbose:

  Should information be printed about how much memory is freed from
  butchering?

- ...:

  Extra arguments possibly used by underlying methods.
