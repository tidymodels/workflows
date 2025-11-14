# Predict from a workflow

This is the [`predict()`](https://rdrr.io/r/stats/predict.html) method
for a fit workflow object. The nice thing about predicting from a
workflow is that it will:

- Preprocess `new_data` using the preprocessing method specified when
  the workflow was created and fit. This is accomplished using
  [`hardhat::forge()`](https://hardhat.tidymodels.org/reference/forge.html),
  which will apply any formula preprocessing or call
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)
  if a recipe was supplied.

- Call
  [`parsnip::predict.model_fit()`](https://parsnip.tidymodels.org/reference/predict.model_fit.html)
  for you using the underlying fit parsnip model.

## Usage

``` r
# S3 method for class 'workflow'
predict(object, new_data, type = NULL, opts = list(), ...)
```

## Arguments

- object:

  A workflow that has been fit by
  [`fit.workflow()`](https://workflows.tidymodels.org/dev/reference/fit-workflow.md)

- new_data:

  A data frame containing the new predictors to preprocess and predict
  on. If using a recipe preprocessor, you should not call
  [`recipes::bake()`](https://recipes.tidymodels.org/reference/bake.html)
  on `new_data` before passing to this function.

- type:

  A single character value or `NULL`. Possible values are `"numeric"`,
  `"class"`, `"prob"`, `"conf_int"`, `"pred_int"`, `"quantile"`,
  `"time"`, `"hazard"`, `"survival"`, or `"raw"`. When `NULL`,
  [`predict()`](https://rdrr.io/r/stats/predict.html) will choose an
  appropriate value based on the model's mode.

- opts:

  A list of optional arguments to the underlying predict function that
  will be used when `type = "raw"`. The list should not include options
  for the model object or the new data being predicted.

- ...:

  Additional `parsnip`-related options, depending on the value of
  `type`. Arguments to the underlying model's prediction function cannot
  be passed here (use the `opts` argument instead). Possible arguments
  are:

  - `interval`: for `type` equal to `"survival"` or `"quantile"`, should
    interval estimates be added, if available? Options are `"none"` and
    `"confidence"`.

  - `level`: for `type` equal to `"conf_int"`, `"pred_int"`, or
    `"survival"`, this is the parameter for the tail area of the
    intervals (e.g. confidence level for confidence intervals). Default
    value is `0.95`.

  - `std_error`: for `type` equal to `"conf_int"` or `"pred_int"`, add
    the standard error of fit or prediction (on the scale of the linear
    predictors). Default value is `FALSE`.

  - `quantile`: for `type` equal to `quantile`, the quantiles of the
    distribution. Default is `(1:9)/10`.

  - `eval_time`: for `type` equal to `"survival"` or `"hazard"`, the
    time points at which the survival probability or hazard is
    estimated.

## Value

A data frame of model predictions, with as many rows as `new_data` has.

## Examples

``` r
library(parsnip)
library(recipes)
library(magrittr)

training <- mtcars[1:20, ]
testing <- mtcars[21:32, ]

model <- linear_reg() |>
  set_engine("lm")

workflow <- workflow() |>
  add_model(model)

recipe <- recipe(mpg ~ cyl + disp, training) |>
  step_log(disp)

workflow <- add_recipe(workflow, recipe)

fit_workflow <- fit(workflow, training)

# This will automatically `bake()` the recipe on `testing`,
# applying the log step to `disp`, and then fit the regression.
predict(fit_workflow, testing)
#> # A tibble: 12 × 1
#>    .pred
#>    <dbl>
#>  1  25.4
#>  2  15.4
#>  3  15.8
#>  4  14.4
#>  5  13.2
#>  6  29.4
#>  7  25.4
#>  8  27.6
#>  9  14.4
#> 10  23.2
#> 11  15.9
#> 12  25.3
```
