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

# cannot add two variables

    Code
      add_variables(workflow, mpg, cyl)
    Condition
      Error in `add_action()`:
      ! A `variables` action has already been added to this workflow.

---

    Code
      add_variables(workflow, variables = workflow_variables(mpg, cyl))
    Condition
      Error in `add_action()`:
      ! A `variables` action has already been added to this workflow.

# can only use a 'xy_blueprint' blueprint

    Code
      add_variables(workflow, mpg, cyl, blueprint = blueprint)
    Condition
      Error in `add_variables()`:
      ! `blueprint` must be a hardhat 'xy_blueprint'.

