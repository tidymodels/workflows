# missing `data` argument has a nice error

    Code
      fit(workflow)
    Condition
      Error in `fit()`:
      ! `data` must be provided to fit a workflow.

# invalid `control` argument has a nice error

    Code
      fit(workflow, mtcars, control = control)
    Condition
      Error in `fit()`:
      ! `control` must be a workflows control object created by `control_workflow()`.

# cannot fit without a pre stage

    Code
      fit(workflow, mtcars)
    Condition
      Error in `.fit_pre()`:
      ! The workflow must have a formula, recipe, or variables preprocessor.
      i Provide one with `add_formula()`, `add_recipe()`, or `add_variables()`.

# cannot fit without a fit stage

    Code
      fit(workflow, mtcars)
    Condition
      Error in `.fit_pre()`:
      ! The workflow must have a model.
      i Provide one with `add_model()`.

# can `predict()` from workflow fit from individual pieces

    Code
      predict(workflow_model, mtcars)
    Condition
      Error in `predict()`:
      ! Can't predict on an untrained workflow.
      i Do you need to call `fit()`?

