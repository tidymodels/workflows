# Add case weights to a workflow

This family of functions revolves around selecting a column of `data` to
use for *case weights*. This column must be one of the allowed case
weight types, such as
[`hardhat::frequency_weights()`](https://hardhat.tidymodels.org/reference/frequency_weights.html)
or
[`hardhat::importance_weights()`](https://hardhat.tidymodels.org/reference/importance_weights.html).
Specifically, it must return `TRUE` from
[`hardhat::is_case_weights()`](https://hardhat.tidymodels.org/reference/is_case_weights.html).
The underlying model will decide whether or not the type of case weights
you have supplied are applicable or not.

- `add_case_weights()` specifies the column that will be interpreted as
  case weights in the model. This column must be present in the `data`
  supplied to
  [fit()](https://workflows.tidymodels.org/dev/reference/fit-workflow.md).

- `remove_case_weights()` removes the case weights. Additionally, if the
  model has already been fit, then the fit is removed.

- `update_case_weights()` first removes the case weights, then replaces
  them with the new ones.

## Usage

``` r
add_case_weights(x, col)

remove_case_weights(x)

update_case_weights(x, col)
```

## Arguments

- x:

  A workflow

- col:

  A single unquoted column name specifying the case weights for the
  model. This must be a classed case weights column, as determined by
  [`hardhat::is_case_weights()`](https://hardhat.tidymodels.org/reference/is_case_weights.html).

## Details

For formula and variable preprocessors, the case weights `col` is
removed from the data before the preprocessor is evaluated. This allows
you to use formulas like `y ~ .` or tidyselection like
[`everything()`](https://tidyselect.r-lib.org/reference/everything.html)
without fear of accidentally selecting the case weights column.

For recipe preprocessors, the case weights `col` is not removed and is
passed along to the recipe. Typically, your recipe will include steps
that can utilize case weights.

## Examples

``` r
library(parsnip)
library(magrittr)
library(hardhat)

mtcars2 <- mtcars
mtcars2$gear <- frequency_weights(mtcars2$gear)

spec <- linear_reg() |>
  set_engine("lm")

wf <- workflow() |>
  add_case_weights(gear) |>
  add_formula(mpg ~ .) |>
  add_model(spec)

wf <- fit(wf, mtcars2)

# Notice that the case weights (gear) aren't included in the predictors
extract_mold(wf)$predictors
#> # A tibble: 32 × 10
#>    `(Intercept)`   cyl  disp    hp  drat    wt  qsec    vs    am  carb
#>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1             1     6  160    110  3.9   2.62  16.5     0     1     4
#>  2             1     6  160    110  3.9   2.88  17.0     0     1     4
#>  3             1     4  108     93  3.85  2.32  18.6     1     1     1
#>  4             1     6  258    110  3.08  3.22  19.4     1     0     1
#>  5             1     8  360    175  3.15  3.44  17.0     0     0     2
#>  6             1     6  225    105  2.76  3.46  20.2     1     0     1
#>  7             1     8  360    245  3.21  3.57  15.8     0     0     4
#>  8             1     4  147.    62  3.69  3.19  20       1     0     2
#>  9             1     4  141.    95  3.92  3.15  22.9     1     0     2
#> 10             1     6  168.   123  3.92  3.44  18.3     1     0     4
#> # ℹ 22 more rows

# Strip them out of the workflow, which also resets the model
remove_case_weights(wf)
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
```
