# Internal workflow functions

`.fit_pre()`, `.fit_model()`, `.fit_post()` and `.fit_finalize()` are
internal workflow functions for *partially* fitting a workflow object.
They are only exported for usage by the tuning package,
[tune](https://github.com/tidymodels/tune), and the general user should
never need to worry about them.

## Usage

``` r
.workflow_postprocessor_requires_fit(workflow)

.fit_pre(workflow, data)

.fit_model(workflow, control)

.fit_post(workflow, data)

.fit_finalize(workflow)
```

## Arguments

- workflow:

  A workflow

  For `.fit_pre()`, this should be a fresh workflow.

  For `.fit_model()`, this should be a workflow that has already been
  trained through `.fit_pre()`.

  For `.fit_post()`, this should be a workflow that has already been
  trained through `.fit_pre()` and `.fit_model()`.

  For `.fit_finalize()`, this should be a workflow that has been through
  both `.fit_pre()` and `.fit_model()`. If the workflow contains an
  optional postprocessor, it should also have been trained through
  `.fit_post()`.

- data:

  A data frame of predictors and outcomes to use when fitting the
  workflow

- control:

  A
  [`control_workflow()`](https://workflows.tidymodels.org/dev/reference/control_workflow.md)
  object

## Examples

``` r
library(parsnip)
library(recipes)
library(magrittr)

model <- linear_reg() |>
  set_engine("lm")

wf_unfit <- workflow() |>
  add_model(model) |>
  add_formula(mpg ~ cyl + log(disp))

wf_fit_pre <- .fit_pre(wf_unfit, mtcars)
wf_fit_model <- .fit_model(wf_fit_pre, control_workflow())
wf_fit <- .fit_finalize(wf_fit_model)

# Notice that fitting through the model doesn't mark the
# workflow as being "trained"
wf_fit_model
#> ══ Workflow ═══════════════════════════════════════════════════════════
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

# Finalizing the workflow marks it as "trained"
wf_fit
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

# Which allows you to predict from it
try(predict(wf_fit_model, mtcars))
#> Error in predict(wf_fit_model, mtcars) : 
#>   Can't predict on an untrained workflow.
#> ℹ Do you need to call `fit()`?

predict(wf_fit, mtcars)
#> # A tibble: 32 × 1
#>    .pred
#>    <dbl>
#>  1  22.0
#>  2  22.0
#>  3  25.8
#>  4  17.8
#>  5  14.5
#>  6  19.0
#>  7  14.5
#>  8  23.1
#>  9  23.4
#> 10  21.6
#> # ℹ 22 more rows
```
