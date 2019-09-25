#' @export
add_recipe <- function(x, recipe, reprep = TRUE) {
  action <- new_action_recipe(recipe, reprep)
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

new_action_recipe <- function(recipe, reprep) {
  if (!is_recipe(recipe)) {
    abort("`recipe` must be a recipe.")
  }

  vec_assert(reprep, ptype = logical(), size = 1L)

  new_action_pre(recipe = recipe, reprep = reprep, subclass = "action_recipe")
}

is_recipe <- function(x) {
  inherits(x, "recipe")
}
