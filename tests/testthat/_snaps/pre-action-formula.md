# formula is validated

    Code
      add_formula(workflow(), 1)
    Condition
      Error in `add_formula()`:
      ! `formula` must be a formula.

# cannot add a formula if a recipe already exists

    Code
      add_formula(workflow, mpg ~ cyl)
    Condition
      Error in `add_formula()`:
      ! A formula cannot be added when a recipe already exists.

# cannot add a formula if variables already exist

    Code
      add_formula(workflow, mpg ~ cyl)
    Condition
      Error in `add_formula()`:
      ! A formula cannot be added when variables already exist.

# cannot add two formulas

    Code
      add_formula(workflow, mpg ~ cyl)
    Condition
      Error in `add_formula()`:
      ! A `formula` action has already been added to this workflow.

# can't pass an `offset()` through `add_formula()` (#162)

    Code
      fit(workflow, data = df)
    Condition
      Error in `fit()`:
      ! Can't use an offset in the formula supplied to `add_formula()`.
      i Instead, specify offsets through a model formula in `add_model(formula = )`.

# can only use a 'formula_blueprint' blueprint

    Code
      add_formula(workflow, mpg ~ cyl, blueprint = blueprint)
    Condition
      Error in `add_formula()`:
      ! `blueprint` must be a hardhat <formula_blueprint>.

