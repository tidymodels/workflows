# recipe is validated

    Code
      add_recipe(workflow(), 1)
    Condition
      Error in `add_recipe()`:
      ! `recipe` must be a recipe.

# cannot add a recipe if a formula already exists

    Code
      add_recipe(workflow, rec)
    Condition
      Error in `add_recipe()`:
      ! A recipe cannot be added when a formula already exists.

# cannot add a recipe if variables already exist

    Code
      add_recipe(workflow, rec)
    Condition
      Error in `add_recipe()`:
      ! A recipe cannot be added when variables already exist.

# cannot add a recipe if recipe is trained

    Code
      add_recipe(workflow, rec)
    Condition
      Error in `add_recipe()`:
      ! Can't add a trained recipe to a workflow.

# cannot add two recipe

    Code
      add_recipe(workflow, rec)
    Condition
      Error in `add_recipe()`:
      ! A `recipe` action has already been added to this workflow.

# can only use a 'recipe_blueprint' blueprint

    Code
      add_recipe(workflow, rec, blueprint = blueprint)
    Condition
      Error in `add_recipe()`:
      ! `blueprint` must be a hardhat <recipe_blueprint>.

