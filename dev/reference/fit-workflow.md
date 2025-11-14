# Fit a workflow object

Fitting a workflow currently involves three main steps:

- Preprocessing the data using a formula preprocessor, or by calling
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html)
  on a recipe.

- Fitting the underlying parsnip model using
  [`parsnip::fit.model_spec()`](https://generics.r-lib.org/reference/fit.html).

- Postprocessing predictions from the model using
  [`tailor::tailor()`](https://tailor.tidymodels.org/reference/tailor.html).

## Usage

``` r
# S3 method for class 'workflow'
fit(object, data, ..., data_calibration = NULL, control = control_workflow())
```

## Arguments

- object:

  A workflow

- data:

  A data frame of predictors and outcomes to use when fitting the
  preprocessor and model.

- ...:

  Not used

- data_calibration:

  A data frame of predictors and outcomes to use when fitting the
  postprocessor. See the "Data Usage" section of
  [`add_tailor()`](https://workflows.tidymodels.org/dev/reference/add_tailor.md)
  for more information.

- control:

  A
  [`control_workflow()`](https://workflows.tidymodels.org/dev/reference/control_workflow.md)
  object

## Value

The workflow `object`, updated with a fit parsnip model in the
`object$fit$fit` slot.

## Indicator Variable Details

Some modeling functions in R create indicator/dummy variables from
categorical data when you use a model formula, and some do not. When you
specify and fit a model with a
[`workflow()`](https://workflows.tidymodels.org/dev/reference/workflow.md),
parsnip and workflows match and reproduce the underlying behavior of the
user-specified model’s computational engine.

### Formula Preprocessor

In the
[modeldata::Sacramento](https://modeldata.tidymodels.org/reference/Sacramento.html)
data set of real estate prices, the `type` variable has three levels:
`"Residential"`, `"Condo"`, and `"Multi-Family"`. This base
[`workflow()`](https://workflows.tidymodels.org/dev/reference/workflow.md)
contains a formula added via
[`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md)
to predict property price from property type, square footage, number of
beds, and number of baths:

    set.seed(123)

    library(parsnip)
    library(recipes)
    library(workflows)
    library(modeldata)

    data("Sacramento")

    base_wf <- workflow() |>
      add_formula(price ~ type + sqft + beds + baths)

This first model does create dummy/indicator variables:

    lm_spec <- linear_reg() |>
      set_engine("lm")

    base_wf |>
      add_model(lm_spec) |>
      fit(Sacramento)

    ## == Workflow [trained] ================================================
    ## Preprocessor: Formula
    ## Model: linear_reg()
    ##
    ## -- Preprocessor ------------------------------------------------------
    ## price ~ type + sqft + beds + baths
    ##
    ## -- Model -------------------------------------------------------------
    ##
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ##
    ## Coefficients:
    ##      (Intercept)  typeMulti_Family   typeResidential
    ##          32919.4          -21995.8           33688.6
    ##             sqft              beds             baths
    ##            156.2          -29788.0            8730.0

There are **five** independent variables in the fitted model for this
OLS linear regression. With this model type and engine, the factor
predictor `type` of the real estate properties was converted to two
binary predictors, `typeMulti_Family` and `typeResidential`. (The third
type, for condos, does not need its own column because it is the
baseline level).

This second model does not create dummy/indicator variables:

    rf_spec <- rand_forest() |>
      set_mode("regression") |>
      set_engine("ranger")

    base_wf |>
      add_model(rf_spec) |>
      fit(Sacramento)

    ## == Workflow [trained] ================================================
    ## Preprocessor: Formula
    ## Model: rand_forest()
    ##
    ## -- Preprocessor ------------------------------------------------------
    ## price ~ type + sqft + beds + baths
    ##
    ## -- Model -------------------------------------------------------------
    ## Ranger result
    ##
    ## Call:
    ##  ranger::ranger(x = maybe_data_frame(x), y = y, num.threads = 1,      verbose = FALSE, seed = sample.int(10^5, 1))
    ##
    ## Type:                             Regression
    ## Number of trees:                  500
    ## Sample size:                      932
    ## Number of independent variables:  4
    ## Mtry:                             2
    ## Target node size:                 5
    ## Variable importance mode:         none
    ## Splitrule:                        variance
    ## OOB prediction error (MSE):       7058847504
    ## R squared (OOB):                  0.5894647

Note that there are **four** independent variables in the fitted model
for this ranger random forest. With this model type and engine,
indicator variables were not created for the `type` of real estate
property being sold. Tree-based models such as random forest models can
handle factor predictors directly, and don’t need any conversion to
numeric binary variables.

### Recipe Preprocessor

When you specify a model with a
[`workflow()`](https://workflows.tidymodels.org/dev/reference/workflow.md)
and a recipe preprocessor via
[`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md),
the *recipe* controls whether dummy variables are created or not; the
recipe overrides any underlying behavior from the model’s computational
engine.

## Examples

``` r
library(parsnip)
library(recipes)
library(magrittr)

model <- linear_reg() |>
  set_engine("lm")

base_wf <- workflow() |>
  add_model(model)

formula_wf <- base_wf |>
  add_formula(mpg ~ cyl + log(disp))

fit(formula_wf, mtcars)
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ cyl + log(disp)
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl  `log(disp)`  
#>     67.6674      -0.1755      -8.7971  
#> 

recipe <- recipe(mpg ~ cyl + disp, mtcars) |>
  step_log(disp)

recipe_wf <- base_wf |>
  add_recipe(recipe)

fit(recipe_wf, mtcars)
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> 1 Recipe Step
#> 
#> • step_log()
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl         disp  
#>     67.6674      -0.1755      -8.7971  
#> 
```
