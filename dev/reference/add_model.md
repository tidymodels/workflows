# Add a model to a workflow

- `add_model()` adds a parsnip model to the workflow.

- `remove_model()` removes the model specification as well as any fitted
  model object. Any extra formulas are also removed.

- `update_model()` first removes the model then adds the new
  specification to the workflow.

## Usage

``` r
add_model(x, spec, ..., formula = NULL)

remove_model(x)

update_model(x, spec, ..., formula = NULL)
```

## Arguments

- x:

  A workflow.

- spec:

  A parsnip model specification.

- ...:

  These dots are for future extensions and must be empty.

- formula:

  An optional formula override to specify the terms of the model.
  Typically, the terms are extracted from the formula or recipe
  preprocessing methods. However, some models (like survival and
  bayesian models) use the formula not to preprocess, but to specify the
  structure of the model. In those cases, a formula specifying the model
  structure must be passed unchanged into the model call itself. This
  argument is used for those purposes.

## Value

`x`, updated with either a new or removed model.

## Details

`add_model()` is a required step to construct a minimal workflow.

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

lm_model <- linear_reg()
lm_model <- set_engine(lm_model, "lm")

regularized_model <- set_engine(lm_model, "glmnet")

workflow <- workflow()
workflow <- add_model(workflow, lm_model)
workflow
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: linear_reg()
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 

workflow <- add_formula(workflow, mpg ~ .)
workflow
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ .
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 

remove_model(workflow)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: None
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ .

fitted <- fit(workflow, data = mtcars)
fitted
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ .
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl         disp           hp         drat  
#>    12.30337     -0.11144      0.01334     -0.02148      0.78711  
#>          wt         qsec           vs           am         gear  
#>    -3.71530      0.82104      0.31776      2.52023      0.65541  
#>        carb  
#>    -0.19942  
#> 

remove_model(fitted)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: None
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ .

remove_model(workflow)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: None
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ .

update_model(workflow, regularized_model)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ .
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: glmnet 
#> 
update_model(fitted, regularized_model)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ .
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: glmnet 
#> 
```
