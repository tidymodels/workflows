# Glance at a workflow model

This is a
[`generics::glance()`](https://generics.r-lib.org/reference/glance.html)
method for a workflow that calls
[`glance()`](https://generics.r-lib.org/reference/glance.html) on the
underlying parsnip model.

`x` must be a trained workflow, resulting in fitted parsnip model to
[`glance()`](https://generics.r-lib.org/reference/glance.html) at.

## Usage

``` r
# S3 method for class 'workflow'
glance(x, ...)
```

## Arguments

- x:

  A workflow

- ...:

  Arguments passed on to methods

## Examples

``` r
if (rlang::is_installed(c("broom", "modeldata"))) {

library(parsnip)
library(magrittr)
library(modeldata)

data("attrition")

model <- logistic_reg() |>
  set_engine("glm")

wf <- workflow() |>
  add_model(model) |>
  add_formula(
    Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime
  )

# Workflow must be trained to call `glance()`
try(glance(wf))

wf_fit <- fit(wf, attrition)

glance(wf_fit)

}
```
