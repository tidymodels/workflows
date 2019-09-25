#' @export
add_recipe <- function(x, recipe) {
  if (!is_recipe(recipe)) {
    abort("`recipe` must be a recipe.")
  }

  action <- new_action_recipe(recipe)

  add_action(x, action)
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
  new_action_pre(recipe = recipe, subclass = "action_recipe")
}

is_recipe <- function(x) {
  inherits(x, "recipe")
}
