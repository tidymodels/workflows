#' @export
add_recipe <- function(x, recipe) {
  action <- new_action_recipe(recipe)
  add_action(x, action, "recipe")
}

# ------------------------------------------------------------------------------

fit.action_recipe <- function(object, director) {
  recipe <- object$recipe
  data <- director$data

  mold <- hardhat::mold(recipe, data)

  new_action <- new_action_recipe(recipe = recipe, mold = mold, done = TRUE)

  director$workflow$pre$actions$recipe <- new_action

  director
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

new_action_recipe <- function(recipe, mold = NULL, done = FALSE) {
  if (!is_recipe(recipe)) {
    abort("`recipe` must be a recipe.")
  }

  new_action_pre(recipe = recipe, mold = mold, done = done, subclass = "action_recipe")
}

is_recipe <- function(x) {
  inherits(x, "recipe")
}
