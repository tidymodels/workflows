# Add variables to a workflow

- `add_variables()` specifies the terms of the model through the usage
  of
  [tidyselect::select_helpers](https://tidyselect.r-lib.org/reference/language.html)
  for the `outcomes` and `predictors`.

- `remove_variables()` removes the variables. Additionally, if the model
  has already been fit, then the fit is removed.

- `update_variables()` first removes the variables, then replaces the
  previous variables with the new ones. Any model that has already been
  fit based on the original variables will need to be refit.

- `workflow_variables()` bundles `outcomes` and `predictors` into a
  single variables object, which can be supplied to `add_variables()`.

## Usage

``` r
add_variables(x, outcomes, predictors, ..., blueprint = NULL, variables = NULL)

remove_variables(x)

update_variables(
  x,
  outcomes,
  predictors,
  ...,
  blueprint = NULL,
  variables = NULL
)

workflow_variables(outcomes, predictors)
```

## Arguments

- x:

  A workflow

- outcomes, predictors:

  Tidyselect expressions specifying the terms of the model. `outcomes`
  is evaluated first, and then all outcome columns are removed from the
  data before `predictors` is evaluated. See
  [tidyselect::select_helpers](https://tidyselect.r-lib.org/reference/language.html)
  for the full range of possible ways to specify terms.

- ...:

  Not used.

- blueprint:

  A hardhat blueprint used for fine tuning the preprocessing.

  If `NULL`,
  [`hardhat::default_xy_blueprint()`](https://hardhat.tidymodels.org/reference/default_xy_blueprint.html)
  is used.

  Note that preprocessing done here is separate from preprocessing that
  might be done by the underlying model.

- variables:

  An alternative specification of `outcomes` and `predictors`, useful
  for supplying variables programmatically.

  - If `NULL`, this argument is unused, and `outcomes` and `predictors`
    are used to specify the variables.

  - Otherwise, this must be the result of calling `workflow_variables()`
    to create a standalone variables object. In this case, `outcomes`
    and `predictors` are completely ignored.

## Value

- `add_variables()` returns `x` with a new variables preprocessor.

- `remove_variables()` returns `x` after resetting any model fit and
  removing the variables preprocessor.

- `update_variables()` returns `x` after removing the variables
  preprocessor, and then re-specifying it with new variables.

- `workflow_variables()` returns a 'workflow_variables' object
  containing both the `outcomes` and `predictors`.

## Details

To fit a workflow, exactly one of
[`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md),
[`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md),
or `add_variables()` *must* be specified.

## Examples

``` r
library(parsnip)

spec_lm <- linear_reg()
spec_lm <- set_engine(spec_lm, "lm")

workflow <- workflow()
workflow <- add_model(workflow, spec_lm)

# Add terms with tidyselect expressions.
# Outcomes are specified before predictors.
workflow1 <- add_variables(
  workflow,
  outcomes = mpg,
  predictors = c(cyl, disp)
)

workflow1 <- fit(workflow1, mtcars)
workflow1
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Variables
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> Outcomes: mpg
#> Predictors: c(cyl, disp)
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl         disp  
#>    34.66099     -1.58728     -0.02058  
#> 

# Removing the variables of a fit workflow will also remove the model
remove_variables(workflow1)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: linear_reg()
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 

# Variables can also be updated
update_variables(workflow1, mpg, starts_with("d"))
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Variables
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> Outcomes: mpg
#> Predictors: starts_with("d")
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> Linear Regression Model Specification (regression)
#> 
#> Computational engine: lm 
#> 

# The `outcomes` are removed before the `predictors` expression
# is evaluated. This allows you to easily specify the predictors
# as "everything except the outcomes".
workflow2 <- add_variables(workflow, mpg, everything())
workflow2 <- fit(workflow2, mtcars)
extract_mold(workflow2)$predictors
#> # A tibble: 32 × 10
#>      cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1     6  160    110  3.9   2.62  16.5     0     1     4     4
#>  2     6  160    110  3.9   2.88  17.0     0     1     4     4
#>  3     4  108     93  3.85  2.32  18.6     1     1     4     1
#>  4     6  258    110  3.08  3.22  19.4     1     0     3     1
#>  5     8  360    175  3.15  3.44  17.0     0     0     3     2
#>  6     6  225    105  2.76  3.46  20.2     1     0     3     1
#>  7     8  360    245  3.21  3.57  15.8     0     0     3     4
#>  8     4  147.    62  3.69  3.19  20       1     0     4     2
#>  9     4  141.    95  3.92  3.15  22.9     1     0     4     2
#> 10     6  168.   123  3.92  3.44  18.3     1     0     4     4
#> # ℹ 22 more rows

# Variables can also be added from the result of a call to
# `workflow_variables()`, which creates a standalone variables object
variables <- workflow_variables(mpg, c(cyl, disp))
workflow3 <- add_variables(workflow, variables = variables)
fit(workflow3, mtcars)
#> ══ Workflow [trained] ═════════════════════════════════════════════════
#> Preprocessor: Variables
#> Model: linear_reg()
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> Outcomes: mpg
#> Predictors: c(cyl, disp)
#> 
#> ── Model ──────────────────────────────────────────────────────────────
#> 
#> Call:
#> stats::lm(formula = ..y ~ ., data = data)
#> 
#> Coefficients:
#> (Intercept)          cyl         disp  
#>    34.66099     -1.58728     -0.02058  
#> 
```
