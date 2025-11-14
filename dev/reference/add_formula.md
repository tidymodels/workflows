# Add formula terms to a workflow

- `add_formula()` specifies the terms of the model through the usage of
  a formula.

- `remove_formula()` removes the formula as well as any downstream
  objects that might get created after the formula is used for
  preprocessing, such as terms. Additionally, if the model has already
  been fit, then the fit is removed.

- `update_formula()` first removes the formula, then replaces the
  previous formula with the new one. Any model that has already been fit
  based on this formula will need to be refit.

## Usage

``` r
add_formula(x, formula, ..., blueprint = NULL)

remove_formula(x)

update_formula(x, formula, ..., blueprint = NULL)
```

## Arguments

- x:

  A workflow

- formula:

  A formula specifying the terms of the model. It is advised to not do
  preprocessing in the formula, and instead use a recipe if that is
  required.

- ...:

  Not used.

- blueprint:

  A hardhat blueprint used for fine tuning the preprocessing.

  If `NULL`,
  [`hardhat::default_formula_blueprint()`](https://hardhat.tidymodels.org/reference/default_formula_blueprint.html)
  is used and is passed arguments that best align with the model present
  in the workflow.

  Note that preprocessing done here is separate from preprocessing that
  might be done by the underlying model. For example, if a blueprint
  with `indicators = "none"` is specified, no dummy variables will be
  created by hardhat, but if the underlying model requires a formula
  interface that internally uses
  [`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html),
  factors will still be expanded to dummy variables by the model.

## Value

`x`, updated with either a new or removed formula preprocessor.

## Details

To fit a workflow, exactly one of `add_formula()`,
[`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md),
or
[`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
*must* be specified.

## Formula Handling

Note that, for different models, the formula given to `add_formula()`
might be handled in different ways, depending on the parsnip model being
used. For example, a random forest model fit using ranger would not
convert any factor predictors to binary indicator variables. This is
consistent with what `ranger::ranger()` would do, but is inconsistent
with what
[`stats::model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
would do.

The documentation for parsnip models provides details about how the data
given in the formula are encoded for the model if they diverge from the
standard [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html)
methodology. Our goal is to be consistent with how the underlying model
package works.

### How is this formula used?

To demonstrate, the example below uses
[`lm()`](https://rdrr.io/r/stats/lm.html) to fit a model. The formula
given to `add_formula()` is used to create the model matrix and that is
what is passed to [`lm()`](https://rdrr.io/r/stats/lm.html) with a
simple formula of `body_mass_g ~ .`:

    library(parsnip)
    library(workflows)
    library(magrittr)
    library(modeldata)
    library(hardhat)

    data(penguins)

    lm_mod <- linear_reg() |>
      set_engine("lm")

    lm_wflow <- workflow() |>
      add_model(lm_mod)

    pre_encoded <- lm_wflow |>
      add_formula(body_mass_g ~ species + island + bill_depth_mm) |>
      fit(data = penguins)

    pre_encoded_parsnip_fit <- pre_encoded |>
      extract_fit_parsnip()

    pre_encoded_fit <- pre_encoded_parsnip_fit$fit

    # The `lm()` formula is *not* the same as the `add_formula()` formula:
    pre_encoded_fit

    ##
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ##
    ## Coefficients:
    ##      (Intercept)  speciesChinstrap     speciesGentoo
    ##        -1009.943             1.328          2236.865
    ##      islandDream   islandTorgersen     bill_depth_mm
    ##            9.221           -18.433           256.913

This can affect how the results are analyzed. For example, to get
sequential hypothesis tests, each individual term is tested:

    anova(pre_encoded_fit)

    ## Analysis of Variance Table
    ##
    ## Response: ..y
    ##                   Df    Sum Sq   Mean Sq  F value Pr(>F)
    ## speciesChinstrap   1  18642821  18642821 141.1482 <2e-16 ***
    ## speciesGentoo      1 128221393 128221393 970.7875 <2e-16 ***
    ## islandDream        1     13399     13399   0.1014 0.7503
    ## islandTorgersen    1       255       255   0.0019 0.9650
    ## bill_depth_mm      1  28051023  28051023 212.3794 <2e-16 ***
    ## Residuals        336  44378805    132080
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Overriding the default encodings

Users can override the model-specific encodings by using a hardhat
blueprint. The blueprint can specify how factors are encoded and whether
intercepts are included. As an example, if you use a formula and would
like the data to be passed to a model untouched:

    minimal <- default_formula_blueprint(indicators = "none", intercept = FALSE)

    un_encoded <- lm_wflow |>
      add_formula(
        body_mass_g ~ species + island + bill_depth_mm,
        blueprint = minimal
      ) |>
      fit(data = penguins)

    un_encoded_parsnip_fit <- un_encoded |>
      extract_fit_parsnip()

    un_encoded_fit <- un_encoded_parsnip_fit$fit

    un_encoded_fit

    ##
    ## Call:
    ## stats::lm(formula = ..y ~ ., data = data)
    ##
    ## Coefficients:
    ##      (Intercept)     bill_depth_mm  speciesChinstrap
    ##        -1009.943           256.913             1.328
    ##    speciesGentoo       islandDream   islandTorgersen
    ##         2236.865             9.221           -18.433

While this looks the same, the raw columns were given to
[`lm()`](https://rdrr.io/r/stats/lm.html) and that function created the
dummy variables. Because of this, the sequential ANOVA tests groups of
parameters to get column-level p-values:

    anova(un_encoded_fit)

    ## Analysis of Variance Table
    ##
    ## Response: ..y
    ##                Df    Sum Sq  Mean Sq F value Pr(>F)
    ## bill_depth_mm   1  48840779 48840779 369.782 <2e-16 ***
    ## species         2 126067249 63033624 477.239 <2e-16 ***
    ## island          2     20864    10432   0.079 0.9241
    ## Residuals     336  44378805   132080
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

### Overriding the default model formula

Additionally, the formula passed to the underlying model can also be
customized. In this case, the `formula` argument of
[`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md)
can be used. To demonstrate, a spline function will be used for the bill
depth:

    library(splines)

    custom_formula <- workflow() |>
      add_model(
        lm_mod,
        formula = body_mass_g ~ species + island + ns(bill_depth_mm, 3)
      ) |>
      add_formula(
        body_mass_g ~ species + island + bill_depth_mm,
        blueprint = minimal
      ) |>
      fit(data = penguins)

    custom_parsnip_fit <- custom_formula |>
      extract_fit_parsnip()

    custom_fit <- custom_parsnip_fit$fit

    custom_fit

    ##
    ## Call:
    ## stats::lm(formula = body_mass_g ~ species + island + ns(bill_depth_mm,
    ##     3), data = data)
    ##
    ## Coefficients:
    ##           (Intercept)       speciesChinstrap          speciesGentoo
    ##              1959.090                  8.534               2352.137
    ##           islandDream        islandTorgersen  ns(bill_depth_mm, 3)1
    ##                 2.425                -12.002               1476.386
    ## ns(bill_depth_mm, 3)2  ns(bill_depth_mm, 3)3
    ##              3187.839               1686.996

### Altering the formula

Finally, when a formula is updated or removed from a fitted workflow,
the corresponding model fit is removed.

    custom_formula_no_fit <- update_formula(custom_formula, body_mass_g ~ species)

    try(extract_fit_parsnip(custom_formula_no_fit))

    ## Error in extract_fit_parsnip(custom_formula_no_fit) :
    ##   Can't extract a model fit from an untrained workflow.
    ## i Do you need to call `fit()`?

## Examples

``` r
workflow <- workflow()
workflow <- add_formula(workflow, mpg ~ cyl)
workflow
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: None
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ cyl

remove_formula(workflow)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: None

update_formula(workflow, mpg ~ disp)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Formula
#> Model: None
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> mpg ~ disp
```
