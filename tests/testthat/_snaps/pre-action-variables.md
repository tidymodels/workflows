# cannot add variables if a recipe already exists

    Code
      add_variables(wf, y, x)
    Condition
      Error in `add_variables()`:
      ! Variables cannot be added when a recipe already exists.

# cannot add variables if a formula already exist

    Code
      add_variables(wf, y, x)
    Condition
      Error in `add_variables()`:
      ! Variables cannot be added when a formula already exists.

# informative error if either `predictors` or `outcomes` aren't provided (#144)

    Code
      add_variables(workflow(), outcomes = mpg)
    Condition
      Error in `workflow_variables()`:
      ! `predictors` can't be missing.

---

    Code
      add_variables(workflow(), predictors = mpg)
    Condition
      Error in `workflow_variables()`:
      ! `outcomes` can't be missing.

# cannot add two variables

    Code
      add_variables(workflow, mpg, cyl)
    Condition
      Error in `add_variables()`:
      ! A `variables` action has already been added to this workflow.

---

    Code
      add_variables(workflow, variables = workflow_variables(mpg, cyl))
    Condition
      Error in `add_variables()`:
      ! A `variables` action has already been added to this workflow.

# can only use a 'xy_blueprint' blueprint

    Code
      add_variables(workflow, mpg, cyl, blueprint = blueprint)
    Condition
      Error in `add_variables()`:
      ! `blueprint` must be a hardhat <xy_blueprint>.

