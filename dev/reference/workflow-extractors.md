# Extract elements of a workflow

**\[soft-deprecated\]**

Please use the `extract_*()` functions instead of these (e.g.
[`extract_mold()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)).

These functions extract various elements from a workflow object. If they
do not exist yet, an error is thrown.

- `pull_workflow_preprocessor()` returns the formula, recipe, or
  variable expressions used for preprocessing.

- `pull_workflow_spec()` returns the parsnip model specification.

- `pull_workflow_fit()` returns the parsnip model fit.

- `pull_workflow_mold()` returns the preprocessed "mold" object returned
  from
  [`hardhat::mold()`](https://hardhat.tidymodels.org/reference/mold.html).
  It contains information about the preprocessing, including either the
  prepped recipe or the formula terms object.

- `pull_workflow_prepped_recipe()` returns the prepped recipe. It is
  extracted from the mold object returned from `pull_workflow_mold()`.

## Usage

``` r
pull_workflow_preprocessor(x)

pull_workflow_spec(x)

pull_workflow_fit(x)

pull_workflow_mold(x)

pull_workflow_prepped_recipe(x)
```

## Arguments

- x:

  A workflow

## Value

The extracted value from the workflow, `x`, as described in the
description section.

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

# The preprocessor is a recipes, formula, or a list holding the
# tidyselect expressions identifying the outcomes/predictors
pull_workflow_preprocessor(recipe_wf)
#> Warning: `pull_workflow_preprocessor()` was deprecated in workflows 0.2.3.
#> ℹ Please use `extract_preprocessor()` instead.
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
pull_workflow_preprocessor(formula_wf)
#> mpg ~ cyl + log(disp)
#> <environment: 0x55bd2b4bd7a0>
pull_workflow_preprocessor(variable_wf)
#> $outcomes
#> <quosure>
#> expr: ^mpg
#> env:  0x55bd2b4bd7a0
#> 
#> $predictors
#> <quosure>
#> expr: ^c(cyl, disp)
#> env:  0x55bd2b4bd7a0
#> 
#> attr(,"class")
#> [1] "workflow_variables"

# The `spec` is the parsnip spec before it has been fit.
# The `fit` is the fit parsnip model.
pull_workflow_spec(fit_formula_wf)
#> Warning: `pull_workflow_spec()` was deprecated in workflows 0.2.3.
#> ℹ Please use `extract_spec_parsnip()` instead.
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 
pull_workflow_fit(fit_formula_wf)
#> Warning: `pull_workflow_fit()` was deprecated in workflows 0.2.3.
#> ℹ Please use `extract_fit_parsnip()` instead.
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

# The mold is returned from `hardhat::mold()`, and contains the
# predictors, outcomes, and information about the preprocessing
# for use on new data at `predict()` time.
pull_workflow_mold(fit_recipe_wf)
#> Warning: `pull_workflow_mold()` was deprecated in workflows 0.2.3.
#> ℹ Please use `extract_mold()` instead.
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

# A useful shortcut is to extract the prepped recipe from the workflow
pull_workflow_prepped_recipe(fit_recipe_wf)
#> Warning: `pull_workflow_prepped_recipe()` was deprecated in workflows 0.2.3.
#> ℹ Please use `extract_recipe()` instead.
#> 
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
  pull_workflow_mold(fit_recipe_wf)$blueprint$recipe,
  pull_workflow_prepped_recipe(fit_recipe_wf)
)
#> [1] TRUE
```
