# Changelog

## workflows (development version)

## workflows 1.3.0

CRAN release: 2025-08-27

- Implemented postprocessing

  - Include a post stage and integrate post-processors from the tailor
    package via
    [`add_tailor()`](https://workflows.tidymodels.org/dev/reference/add_tailor.md),
    [`remove_tailor()`](https://workflows.tidymodels.org/dev/reference/add_tailor.md),
    [`update_tailor()`](https://workflows.tidymodels.org/dev/reference/add_tailor.md),
    [`extract_postprocessor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html),
    and
    [`.fit_post()`](https://workflows.tidymodels.org/dev/reference/workflows-internals.md)
    ([\#225](https://github.com/tidymodels/workflows/issues/225)).
  - Include post-processing in the workflow methods for
    [`augment()`](https://generics.r-lib.org/reference/augment.html)
    ([\#276](https://github.com/tidymodels/workflows/issues/276)),
    [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
    ([\#266](https://github.com/tidymodels/workflows/issues/266)),
    [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
    ([\#305](https://github.com/tidymodels/workflows/issues/305)),
    [`required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
    ([\#299](https://github.com/tidymodels/workflows/issues/299)),
    [`tunable()`](https://generics.r-lib.org/reference/tunable.html)
    ([\#272](https://github.com/tidymodels/workflows/issues/272)), and
    [`tune_args()`](https://generics.r-lib.org/reference/tune_args.html)
    ([\#270](https://github.com/tidymodels/workflows/issues/270)).
  - [`extract_tailor()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
    extracts the tailor from a workflow
    ([\#301](https://github.com/tidymodels/workflows/issues/301)).
  - Checks on compatibility of model spec and tailor
    ([\#304](https://github.com/tidymodels/workflows/issues/304)).

- Increased the minimum required R version to R 4.1.

- Updated auto toggle sparsity to handle lightgbm engine
  ([\#290](https://github.com/tidymodels/workflows/issues/290)).

## workflows 1.2.0

CRAN release: 2025-02-18

### New features

- Enable fitting and prediction with sparse data.
  - [`fit()`](https://generics.r-lib.org/reference/fit.html) can now
    take dgCMatrix and sparse tibbles as data values when
    [`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md)
    or
    [`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
    is used
    ([\#245](https://github.com/tidymodels/workflows/issues/245),
    [\#258](https://github.com/tidymodels/workflows/issues/258)).
  - [`predict()`](https://rdrr.io/r/stats/predict.html) can now take
    dgCMatrix and sparse tibble input for `new_data` argument
    ([\#261](https://github.com/tidymodels/workflows/issues/261)).
- [`extract_fit_time()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  returns the time it took to train the workflow
  ([\#191](https://github.com/tidymodels/workflows/issues/191)).

### Bug fixes and minor improvements

- Transition package warnings and errors to use cli instead of rlang
  ([\#241](https://github.com/tidymodels/workflows/issues/241)).

- Minimum R version bumped to 4.0.0.

- Added reference to
  [`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
  in `stages` vignette ([@brshallo](https://github.com/brshallo),
  [\#190](https://github.com/tidymodels/workflows/issues/190)). \#
  workflows 1.1.4

- While
  [`augment.workflow()`](https://workflows.tidymodels.org/dev/reference/augment.workflow.md)
  previously never returned a `.resid` column, the method will now
  return residuals under the same conditions that `augment.model_fit()`
  does ([\#201](https://github.com/tidymodels/workflows/issues/201)).

- [`augment.workflow()`](https://workflows.tidymodels.org/dev/reference/augment.workflow.md)
  gained an `eval_time` argument, enabling augmenting censored
  regression models
  ([\#200](https://github.com/tidymodels/workflows/issues/200),
  [\#213](https://github.com/tidymodels/workflows/issues/213)).

- The prediction columns are now appended to the LHS rather than RHS of
  `new_data` in
  [`augment.workflow()`](https://workflows.tidymodels.org/dev/reference/augment.workflow.md),
  following analogous changes in parsnip
  ([\#200](https://github.com/tidymodels/workflows/issues/200)).

- Each of the `pull_*()` functions soft-deprecated in workflows v0.2.3
  now warn on every usage
  ([\#198](https://github.com/tidymodels/workflows/issues/198)).

- [`add_recipe()`](https://workflows.tidymodels.org/dev/reference/add_recipe.md)
  will now error informatively when supplied a trained recipe
  ([\#179](https://github.com/tidymodels/workflows/issues/179)).

## workflows 1.1.3

CRAN release: 2023-02-22

- The workflows methods for
  [`generics::tune_args()`](https://generics.r-lib.org/reference/tune_args.html)
  and
  [`generics::tunable()`](https://generics.r-lib.org/reference/tunable.html)
  are now registered unconditionally
  ([\#192](https://github.com/tidymodels/workflows/issues/192)).

## workflows 1.1.2

CRAN release: 2022-11-16

- Tightens integration with parsnip’s machinery for checking that needed
  parsnip extension packages are loaded.
  [`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md)
  will now error if a model specification is supplied that requires a
  missing extension package
  ([\#184](https://github.com/tidymodels/workflows/issues/184)).

- Introduces support for unsupervised model specifications via the
  modelenv package
  ([\#180](https://github.com/tidymodels/workflows/issues/180)).

## workflows 1.1.0

CRAN release: 2022-09-26

- Simon Couch is now the maintainer
  ([\#170](https://github.com/tidymodels/workflows/issues/170)).

- [`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md)
  now errors if you try to add a model specification that contains an
  unknown mode. This is a breaking change, as previously in some cases
  it would successfully “guess” the mode. This change brings workflows
  more in line with
  [`parsnip::fit()`](https://generics.r-lib.org/reference/fit.html) and
  [`parsnip::fit_xy()`](https://generics.r-lib.org/reference/fit_xy.html)
  ([\#160](https://github.com/tidymodels/workflows/issues/160),
  tidymodels/parsnip#801).

- `broom::augment()` now works correctly in the edge case where you had
  supplied a hardhat blueprint with `composition` set to either
  `"matrix"` or `"dgCMatrix"`
  ([\#148](https://github.com/tidymodels/workflows/issues/148)).

- [`butcher::axe_fitted()`](https://butcher.tidymodels.org/reference/axe_fitted.html)
  now axes the recipe preprocessor that is stored inside a workflow,
  which will reduce the size of the `template` data frame that is stored
  in the recipe
  ([\#147](https://github.com/tidymodels/workflows/issues/147)).

- [`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md)
  no longer silently ignores offsets supplied with
  [`offset()`](https://rdrr.io/r/stats/offset.html). Instead, it now
  errors at [`fit()`](https://generics.r-lib.org/reference/fit.html)
  time with a message that encourages you to use a model formula through
  `add_model(formula = )` instead
  ([\#162](https://github.com/tidymodels/workflows/issues/162)).

## workflows 1.0.0

CRAN release: 2022-07-05

- New
  [`add_case_weights()`](https://workflows.tidymodels.org/dev/reference/add_case_weights.md),
  [`update_case_weights()`](https://workflows.tidymodels.org/dev/reference/add_case_weights.md),
  and
  [`remove_case_weights()`](https://workflows.tidymodels.org/dev/reference/add_case_weights.md)
  for specifying a column to use as case weights which will be passed on
  to the underlying parsnip model
  ([\#118](https://github.com/tidymodels/workflows/issues/118)).

- R \>=3.4.0 is now required, in line with the rest of the tidyverse.

## workflows 0.2.6

CRAN release: 2022-03-18

- Fixed tests that relied on an incorrect assumption about the version
  of tune that is installed.

## workflows 0.2.5

CRAN release: 2022-03-16

- Improved error message in
  [`workflow_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
  if either `outcomes` or `predictors` are missing
  ([\#144](https://github.com/tidymodels/workflows/issues/144)).

- Removed ellipsis dependency in favor of equivalent functions in rlang.

- New
  [`extract_parameter_set_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  and
  [`extract_parameter_dials()`](https://hardhat.tidymodels.org/reference/hardhat-extract.html)
  methods to extract parameter sets and single parameters from
  `workflow` objects.

## workflows 0.2.4

CRAN release: 2021-10-12

- [`add_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md)
  and
  [`update_model()`](https://workflows.tidymodels.org/dev/reference/add_model.md)
  now use `...` to separate the required arguments from the optional
  arguments, forcing optional arguments to be named. This change was
  made to make it easier for us to extend these functions with new
  arguments in the future.

- The workflows method for
  [`generics::required_pkgs()`](https://generics.r-lib.org/reference/required_pkgs.html)
  is now registered unconditionally
  ([\#121](https://github.com/tidymodels/workflows/issues/121)).

- Internally cleaned up remaining usage of soft-deprecated `pull_*()`
  functions.

## workflows 0.2.3

CRAN release: 2021-07-15

- [`workflow()`](https://workflows.tidymodels.org/dev/reference/workflow.md)
  has gained new `preprocessor` and `spec` arguments for adding a
  preprocessor (such as a recipe or formula) and a parsnip model
  specification directly to a workflow upon creation. In many cases,
  this can reduce the lines of code required to construct a complete
  workflow
  ([\#108](https://github.com/tidymodels/workflows/issues/108)).

- New `extract_*()` functions have been added that supersede the
  existing `pull_*()` functions. This is part of a larger move across
  the tidymodels packages towards a family of generic `extract_*()`
  functions. The `pull_*()` functions have been soft-deprecated, and
  will eventually be removed
  ([\#106](https://github.com/tidymodels/workflows/issues/106)).

## workflows 0.2.2

CRAN release: 2021-03-10

- [`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
  now allows for specifying a bundle of model terms through
  `add_variables(variables = )`, supplying a pre-created set of
  variables with the new
  [`workflow_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
  helper. This is useful for supplying a set of variables
  programmatically
  ([\#92](https://github.com/tidymodels/workflows/issues/92)).

- New
  [`is_trained_workflow()`](https://workflows.tidymodels.org/dev/reference/is_trained_workflow.md)
  for determining if a workflow has already been trained through a call
  to [`fit()`](https://generics.r-lib.org/reference/fit.html)
  ([\#91](https://github.com/tidymodels/workflows/issues/91)).

- [`fit()`](https://generics.r-lib.org/reference/fit.html) now errors
  immediately if `control` is not created by
  [`control_workflow()`](https://workflows.tidymodels.org/dev/reference/control_workflow.md)
  ([\#89](https://github.com/tidymodels/workflows/issues/89)).

- Added `broom::augment()` and `broom::glance()` methods for trained
  workflow objects
  ([\#76](https://github.com/tidymodels/workflows/issues/76)).

- Added support for butchering a workflow using
  [`butcher::butcher()`](https://butcher.tidymodels.org/reference/butcher.html).

- Updated to testthat 3.0.0.

## workflows 0.2.1

CRAN release: 2020-10-08

- New
  [`.fit_finalize()`](https://workflows.tidymodels.org/dev/reference/workflows-internals.md)
  for internal usage by the tune package.

## workflows 0.2.0

CRAN release: 2020-09-15

- New
  [`add_variables()`](https://workflows.tidymodels.org/dev/reference/add_variables.md)
  for specifying model terms using tidyselect expressions with no extra
  preprocessing. For example:

      wf <- workflow() %>%
        add_variables(y, c(var1, start_with("x_"))) %>%
        add_model(spec_lm)

  One benefit of specifying terms in this way over the formula method is
  to avoid preprocessing from
  [`model.matrix()`](https://rdrr.io/r/stats/model.matrix.html), which
  might strip the class of your predictor columns (as it does with Date
  columns) ([\#34](https://github.com/tidymodels/workflows/issues/34)).

## workflows 0.1.3

CRAN release: 2020-08-10

- A test has been updated to reflect a change in parsnip 0.1.3 regarding
  how intercept columns are removed during prediction
  ([\#65](https://github.com/tidymodels/workflows/issues/65)).

## workflows 0.1.2

CRAN release: 2020-07-07

- When using a formula preprocessor with
  [`add_formula()`](https://workflows.tidymodels.org/dev/reference/add_formula.md),
  workflows now uses model-specific information from parsnip to decide
  whether to expand factors via dummy encoding (`n - 1` levels), one-hot
  encoding (`n` levels), or no expansion at all. This should result in
  more intuitive behavior when working with models that don’t require
  dummy variables. For example, if a parsnip
  [`rand_forest()`](https://parsnip.tidymodels.org/reference/rand_forest.html)
  model is used with a ranger engine, dummy variables will not be
  created, because ranger can handle factors directly
  ([\#51](https://github.com/tidymodels/workflows/issues/51),
  [\#53](https://github.com/tidymodels/workflows/issues/53)).

## workflows 0.1.1

CRAN release: 2020-03-17

- hardhat’s minimum required version has been bumped to 0.1.2, as it
  contains an important fix to how recipes are prepped by default.

## workflows 0.1.0

CRAN release: 2019-12-30

- Added a `NEWS.md` file to track changes to the package.
