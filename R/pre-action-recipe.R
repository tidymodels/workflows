#' Add a recipe to a workflow
#'
#' `add_recipe()` specifies the terms of the model and any preprocessing that
#' is required through the usage of a recipe.
#'
#' To fit a workflow, one of `add_formula()` or `add_recipe()` _must_ be
#' specified, but not both.
#'
#' @param x A workflow.
#'
#' @param recipe A recipe created using [recipes::recipe()].
#'
#' @export
add_recipe <- function(x, recipe) {
  action <- new_action_recipe(recipe)
  add_action(x, action, "recipe")
}

# ------------------------------------------------------------------------------

fit.action_recipe <- function(object, workflow, data) {
  recipe <- object$recipe

  workflow$pre$mold <- hardhat::mold(recipe, data)

  # All pre steps return the `workflow` and `data`
  list(workflow = workflow, data = data)
}

# ------------------------------------------------------------------------------

check_conflicts.action_recipe <- function(action, x) {
  pre <- x$pre

  if (has_action(pre, "formula")) {
    abort("A recipe cannot be added when a formula already exists.")
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_recipe <- function(recipe) {
  if (!is_recipe(recipe)) {
    abort("`recipe` must be a recipe.")
  }

  new_action_pre(recipe = recipe, subclass = "action_recipe")
}

is_recipe <- function(x) {
  inherits(x, "recipe")
}
