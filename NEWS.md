# workflows 1.1.4

* While `augment.workflow()` previously never returned a `.resid` column, the 
  method will now return residuals under the same conditions that
  `augment.model_fit()` does (#201).

* `augment.workflow()` gained an `eval_time` argument, enabling augmenting
  censored regression models (#200, #213).

* The prediction columns are now appended to the LHS rather than RHS of 
  `new_data` in `augment.workflow()`, following analogous changes in 
   parsnip (#200).

* Each of the `pull_*()` functions soft-deprecated in workflows v0.2.3 
  now warn on every usage (#198). 
  
* `add_recipe()` will now error informatively when supplied a trained recipe
  (#179).

# workflows 1.1.3

* The workflows methods for `generics::tune_args()` and `generics::tunable()`
  are now registered unconditionally (#192).

# workflows 1.1.2

* Tightens integration with parsnip's machinery for checking that needed 
  parsnip extension packages are loaded. `add_model()` will now error if a model
  specification is supplied that requires a missing extension package (#184).
  
* Introduces support for unsupervised model specifications via the modelenv
  package (#180). 

# workflows 1.1.0

* Simon Couch is now the maintainer (#170).

* `add_model()` now errors if you try to add a model specification
  that contains an unknown mode. This is a breaking change, as previously in
  some cases it would successfully "guess" the mode. This change brings
  workflows more in line with `parsnip::fit()` and `parsnip::fit_xy()`
  (#160, tidymodels/parsnip#801).

* `broom::augment()` now works correctly in the edge case where you had supplied
  a hardhat blueprint with `composition` set to either `"matrix"` or
  `"dgCMatrix"` (#148).

* `butcher::axe_fitted()` now axes the recipe preprocessor that is stored inside
  a workflow, which will reduce the size of the `template` data frame that is
  stored in the recipe (#147).

* `add_formula()` no longer silently ignores offsets supplied with `offset()`.
  Instead, it now errors at `fit()` time with a message that encourages you to
  use a model formula through `add_model(formula = )` instead (#162).

# workflows 1.0.0

* New `add_case_weights()`, `update_case_weights()`, and `remove_case_weights()`
  for specifying a column to use as case weights which will be passed on to the
  underlying parsnip model (#118).

* R >=3.4.0 is now required, in line with the rest of the tidyverse.

# workflows 0.2.6

* Fixed tests that relied on an incorrect assumption about the version of tune
  that is installed.

# workflows 0.2.5

* Improved error message in `workflow_variables()` if either `outcomes` or
  `predictors` are missing (#144).

* Removed ellipsis dependency in favor of equivalent functions in rlang.

* New `extract_parameter_set_dials()` and `extract_parameter_dials()` methods 
  to extract parameter sets and single parameters from `workflow` objects.

# workflows 0.2.4

* `add_model()` and `update_model()` now use `...` to separate the required
  arguments from the optional arguments, forcing optional arguments to be
  named. This change was made to make it easier for us to extend these functions
  with new arguments in the future.
  
* The workflows method for `generics::required_pkgs()` is now registered
  unconditionally (#121).

* Internally cleaned up remaining usage of soft-deprecated `pull_*()` functions.

# workflows 0.2.3

* `workflow()` has gained new `preprocessor` and `spec` arguments for adding
  a preprocessor (such as a recipe or formula) and a parsnip model specification
  directly to a workflow upon creation. In many cases, this can reduce the
  lines of code required to construct a complete workflow (#108).
  
* New `extract_*()` functions have been added that supersede the existing
  `pull_*()` functions. This is part of a larger move across the tidymodels
  packages towards a family of generic `extract_*()` functions. The `pull_*()`
  functions have been soft-deprecated, and will eventually be removed (#106).

# workflows 0.2.2
  
* `add_variables()` now allows for specifying a bundle of model terms through
  `add_variables(variables = )`, supplying a pre-created set of variables with
  the new `workflow_variables()` helper. This is useful for supplying a set
  of variables programmatically (#92).

* New `is_trained_workflow()` for determining if a workflow has already been
  trained through a call to `fit()` (#91).

* `fit()` now errors immediately if `control` is not created by
  `control_workflow()` (#89).

* Added `broom::augment()` and `broom::glance()` methods for trained workflow
  objects (#76).

* Added support for butchering a workflow using `butcher::butcher()`.

* Updated to testthat 3.0.0.

# workflows 0.2.1

* New `.fit_finalize()` for internal usage by the tune package.

# workflows 0.2.0

* New `add_variables()` for specifying model terms using tidyselect expressions
  with no extra preprocessing. For example:
  
  ```
  wf <- workflow() %>%
    add_variables(y, c(var1, start_with("x_"))) %>%
    add_model(spec_lm)
  ```
  
  One benefit of specifying terms in this way over the formula method is to
  avoid preprocessing from `model.matrix()`, which might strip the class of
  your predictor columns (as it does with Date columns) (#34).

# workflows 0.1.3

* A test has been updated to reflect a change in parsnip 0.1.3 regarding how
  intercept columns are removed during prediction (#65).

# workflows 0.1.2

* When using a formula preprocessor with `add_formula()`, workflows now uses
  model-specific information from parsnip to decide whether to expand
  factors via dummy encoding (`n - 1` levels), one-hot encoding (`n` levels), or
  no expansion at all. This should result in more intuitive behavior when
  working with models that don't require dummy variables. For example, if a
  parsnip `rand_forest()` model is used with a ranger engine, dummy variables
  will not be created, because ranger can handle factors directly (#51, #53).

# workflows 0.1.1

* hardhat's minimum required version has been bumped to 0.1.2, as it contains
  an important fix to how recipes are prepped by default.

# workflows 0.1.0

* Added a `NEWS.md` file to track changes to the package.
