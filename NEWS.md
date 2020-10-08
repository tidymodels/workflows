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
