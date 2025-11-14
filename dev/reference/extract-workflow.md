# Extract elements of a workflow

These functions extract various elements from a workflow object. If they
do not exist yet, an error is thrown.

- [`extract_preprocessor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the formula, recipe, or variable expressions used for
  preprocessing.

- [`extract_spec_parsnip()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parsnip model specification.

- [`extract_fit_parsnip()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the parsnip model fit object.

- [`extract_fit_engine()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the engine specific fit embedded within a parsnip model fit.
  For example, when using
  [`parsnip::linear_reg()`](https://parsnip.tidymodels.org/reference/linear_reg.html)
  with the `"lm"` engine, this returns the underlying `lm` object.

- [`extract_mold()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the preprocessed "mold" object returned from
  [`hardhat::mold()`](https://hardhat.tidymodels.org/reference/mold.html).
  It contains information about the preprocessing, including either the
  prepped recipe, the formula terms object, or variable selectors.

- [`extract_recipe()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the recipe. The `estimated` argument specifies whether the
  fitted or original recipe is returned.

- [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns a single dials parameter object.

- [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns a set of dials parameter objects.

- [`extract_fit_time()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns a tibble with elapsed fit times. The fit times correspond to
  the time for the parsnip engine or recipe steps to fit (or their sum
  if `summarize = TRUE`) and do not include other portions of the
  elapsed time in
  [`fit.workflow()`](https://workflows.tidymodels.org/dev/reference/fit-workflow.md).

- [`extract_postprocessor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the postprocessor object.

## Usage

``` r
# S3 method for class 'workflow'
extract_spec_parsnip(x, ...)

# S3 method for class 'workflow'
extract_recipe(x, ..., estimated = TRUE)

# S3 method for class 'workflow'
extract_fit_parsnip(x, ...)

# S3 method for class 'workflow'
extract_fit_engine(x, ...)

# S3 method for class 'workflow'
extract_mold(x, ...)

# S3 method for class 'workflow'
extract_preprocessor(x, ...)

# S3 method for class 'workflow'
extract_postprocessor(x, ..., estimated = FALSE)

# S3 method for class 'workflow'
extract_tailor(x, ..., estimated = TRUE)

# S3 method for class 'workflow'
extract_parameter_set_dials(x, ...)

# S3 method for class 'workflow'
extract_parameter_dials(x, parameter, ...)

# S3 method for class 'workflow'
extract_fit_time(x, summarize = TRUE, ...)
```

## Arguments

- x:

  A workflow

- ...:

  Not currently used.

- estimated:

  A logical for whether the original (unfit) recipe or the fitted recipe
  should be returned. This argument should be named.

- parameter:

  A single string for the parameter ID.

- summarize:

  A logical for whether the elapsed fit time should be returned as a
  single row or multiple rows.

## Value

The extracted value from the object, `x`, as described in the
description section.

## Details

Extracting the underlying engine fit can be helpful for describing the
model (via [`print()`](https://rdrr.io/r/base/print.html),
[`summary()`](https://rdrr.io/r/base/summary.html),
[`plot()`](https://rdrr.io/r/graphics/plot.default.html), etc.) or for
variable importance/explainers.

However, users should not invoke the
[`predict()`](https://rdrr.io/r/stats/predict.html) method on an
extracted model. There may be preprocessing operations that `workflows`
has executed on the data prior to giving it to the model. Bypassing
these can lead to errors or silently generating incorrect predictions.

*Good*:

    workflow_fit |> predict(new_data)

*Bad*:

    workflow_fit |> extract_fit_engine()  |> predict(new_data)
    # or
    workflow_fit |> extract_fit_parsnip() |> predict(new_data)

## Examples

``` r
library(parsnip)
library(recipes)
library(magrittr)

model <- linear_reg() |>
  set_engine("lm")

recipe <- recipe(mpg ~ cyl + disp, mtcars) |>
  step_log(disp)

base_wf <- workflow() |>
  add_model(model)

recipe_wf <- add_recipe(base_wf, recipe)
formula_wf <- add_formula(base_wf, mpg ~ cyl + log(disp))
variable_wf <- add_variables(base_wf, mpg, c(cyl, disp))

fit_recipe_wf <- fit(recipe_wf, mtcars)
fit_formula_wf <- fit(formula_wf, mtcars)

# The preprocessor is a recipe, formula, or a list holding the
# tidyselect expressions identifying the outcomes/predictors
extract_preprocessor(recipe_wf)
#> 
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 2
#> 
#> ── Operations 
#> • Log transformation on: disp
extract_preprocessor(formula_wf)
#> mpg ~ cyl + log(disp)
#> <environment: 0x556a622fea38>
extract_preprocessor(variable_wf)
#> $outcomes
#> <quosure>
#> expr: ^mpg
#> env:  0x556a622fea38
#> 
#> $predictors
#> <quosure>
#> expr: ^c(cyl, disp)
#> env:  0x556a622fea38
#> 
#> attr(,"class")
#> [1] "workflow_variables"

# The `spec` is the parsnip spec before it has been fit.
# The `fit` is the fitted parsnip model.
extract_spec_parsnip(fit_formula_wf)
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
extract_fit_parsnip(fit_formula_wf)
#> parsnip model object
#> 
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl  `log(disp)`  
#>     67.6674      -0.1755      -8.7971  
#> 
extract_fit_engine(fit_formula_wf)
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl  `log(disp)`  
#>     67.6674      -0.1755      -8.7971  
#> 

# The mold is returned from `hardhat::mold()`, and contains the
# predictors, outcomes, and information about the preprocessing
# for use on new data at `predict()` time.
extract_mold(fit_recipe_wf)
#> $predictors
#> # A tibble: 32 × 2
#>      cyl  disp
#>    <dbl> <dbl>
#>  1     6  5.08
#>  2     6  5.08
#>  3     4  4.68
#>  4     6  5.55
#>  5     8  5.89
#>  6     6  5.42
#>  7     8  5.89
#>  8     4  4.99
#>  9     4  4.95
#> 10     6  5.12
#> # ℹ 22 more rows
#> 
#> $outcomes
#> # A tibble: 32 × 1
#>      mpg
#>    <dbl>
#>  1  21  
#>  2  21  
#>  3  22.8
#>  4  21.4
#>  5  18.7
#>  6  18.1
#>  7  14.3
#>  8  24.4
#>  9  22.8
#> 10  19.2
#> # ℹ 22 more rows
#> 
#> $blueprint
#> Recipe blueprint:
#> # Predictors: 2
#> # Outcomes: 1
#> Intercept: FALSE
#> Novel Levels: FALSE
#> Composition: tibble
#> 
#> 
#> $extras
#> $extras$roles
#> NULL
#> 
#> 

# A useful shortcut is to extract the fitted recipe from the workflow
extract_recipe(fit_recipe_wf)
#> ── Recipe ─────────────────────────────────────────────────────────────
#> 
#> ── Inputs 
#> Number of variables by role
#> outcome:   1
#> predictor: 2
#> 
#> ── Training information 
#> Training data contained 32 data points and no incomplete rows.
#> 
#> ── Operations 
#> • Log transformation on: disp | Trained

# That is identical to
identical(
  extract_mold(fit_recipe_wf)$blueprint$recipe,
  extract_recipe(fit_recipe_wf)
)
#> [1] TRUE
```
