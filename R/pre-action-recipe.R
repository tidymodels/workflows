#' Add a recipe to a workflow
#'
#' `add_recipe()` specifies the terms of the model and any preprocessing that
#' is required through the usage of a recipe.
#'
#' `remove_recipe()` removes the recipe as well as any objects related to the
#'  application of the recipe to the data (e.g., the prepared recipe, if it
#'  exists), as well as any fitted model that used the recipe.
#'
#' `update_recipe()` first removes the recipe, then replaces the previous
#'  recipe with the new one. Any fitted model based on this recipe will need
#'  to be refit.
#'
#' To fit a workflow, one of `add_formula()` or `add_recipe()` _must_ be
#' specified, but not both.
#'
#' @param x A workflow
#'
#' @param recipe A recipe created using [recipes::recipe()]
#'
#' @export
#' @examples
#' library(recipes)
#'
#' recipe <- recipe(mpg ~ cyl, mtcars)
#' recipe <- step_log(recipe, cyl)
#'
#' workflow <- workflow()
#' workflow <- add_recipe(workflow, recipe)
#' workflow
#'
#' remove_recipe(workflow)
#'
#' update_recipe(workflow, recipe(mpg ~ cyl, mtcars))
#'
add_recipe <- function(x, recipe) {
  validate_recipes_available()
  action <- new_action_recipe(recipe)
  add_action(x, action, "recipe")
}

#' @rdname add_recipe
#' @export
remove_recipe <- function(x) {
  check_existing_object(x, "recipe")
  new_workflow(
    pre = new_stage_pre(),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_recipe
#' @export
update_recipe <- function(x, recipe) {
  x <- remove_recipe(x)
  add_recipe(x, recipe)
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
