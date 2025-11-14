# Augment data with predictions

This is a
[`generics::augment()`](https://generics.r-lib.org/reference/augment.html)
method for a workflow that calls
[`augment()`](https://generics.r-lib.org/reference/augment.html) on the
underlying parsnip model with `new_data`.

`x` must be a trained workflow, resulting in fitted parsnip model to
[`augment()`](https://generics.r-lib.org/reference/augment.html) with.

`new_data` will be preprocessed using the preprocessor in the workflow,
and that preprocessed data will be used to generate predictions. The
final result will contain the original `new_data` with new columns
containing the prediction information.

## Usage

``` r
# S3 method for class 'workflow'
augment(x, new_data, eval_time = NULL, ...)
```

## Arguments

- x:

  A workflow

- new_data:

  A data frame of predictors

- eval_time:

  For censored regression models, a vector of time points at which the
  survival probability is estimated. See
  [`parsnip::augment.model_fit()`](https://generics.r-lib.org/reference/augment.html)
  for more details.

- ...:

  Arguments passed on to methods

## Value

`new_data` with new prediction specific columns.

## Examples

``` r
if (rlang::is_installed("broom")) {

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

wf_fit <- fit(wf, attrition)

augment(wf_fit, attrition)

}
```
