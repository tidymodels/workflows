# Add a recipe to a workflow

- `add_recipe()` specifies the terms of the model and any preprocessing
  that is required through the usage of a recipe.

- `remove_recipe()` removes the recipe as well as any downstream objects
  that might get created after the recipe is used for preprocessing,
  such as the prepped recipe. Additionally, if the model has already
  been fit, then the fit is removed.

- `update_recipe()` first removes the recipe, then replaces the previous
  recipe with the new one. Any model that has already been fit based on
  this recipe will need to be refit.

## Usage

``` r
add_recipe(x, recipe, ..., blueprint = NULL)

remove_recipe(x)

update_recipe(x, recipe, ..., blueprint = NULL)
```

## Arguments

- x:

  A workflow

- recipe:

  A recipe created using
  [`recipes::recipe()`](https://recipes.tidymodels.org/reference/recipe.html).
  The recipe should not have been trained already with
  [`recipes::prep()`](https://recipes.tidymodels.org/reference/prep.html);
  workflows will handle training internally.

- ...:

  Not used.

- blueprint:

  A hardhat blueprint used for fine tuning the preprocessing.

  If `NULL`,
  [`hardhat::default_recipe_blueprint()`](https://hardhat.tidymodels.org/reference/default_recipe_blueprint.html)
  is used.

  Note that preprocessing done here is separate from preprocessing that
  might be done automatically by the underlying model.

## Value

`x`, updated with either a new or removed recipe preprocessor.

## Details

To fit a workflow, exactly one of
[`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md),
`add_recipe()`, or
[`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
*must* be specified.

## Examples

``` r
library(recipes)
#> Loading required package: dplyr
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
#> 
#> Attaching package: ‘recipes’
#> The following object is masked from ‘package:stats’:
#> 
#>     step
library(magrittr)

recipe <- recipe(mpg ~ cyl, mtcars) |>
  step_log(cyl)

workflow <- workflow() |>
  add_recipe(recipe)

workflow
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: None
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> 1 Recipe Step
#> 
#> • step_log()

remove_recipe(workflow)
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: None
#> Model: None

update_recipe(workflow, recipe(mpg ~ cyl, mtcars))
#> ══ Workflow ═══════════════════════════════════════════════════════════
#> Preprocessor: Recipe
#> Model: None
#> 
#> ── Preprocessor ───────────────────────────────────────────────────────
#> 0 Recipe Steps
```
