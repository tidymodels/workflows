# Determine if a workflow has been trained

A trained workflow is one that has gone through
[`fit()`](https://workflows.tidymodels.org/dev/reference/fit-workflow.md),
which preprocesses the underlying data, and fits the parsnip model.

## Usage

``` r
is_trained_workflow(x)
```

## Arguments

- x:

  A workflow.

## Value

A single logical indicating if the workflow has been trained or not.

## Examples

``` r
library(parsnip)
library(recipes)
library(magrittr)

rec <- recipe(mpg ~ cyl, mtcars)

mod <- linear_reg()
mod <- set_engine(mod, "lm")

wf <- workflow() |>
  add_recipe(rec) |>
  add_model(mod)

# Before any preprocessing or model fitting has been done
is_trained_workflow(wf)
#> [1] FALSE

wf <- fit(wf, mtcars)

# After all preprocessing and model fitting
is_trained_workflow(wf)
#> [1] TRUE
```
