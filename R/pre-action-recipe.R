#' @export
add_recipe <- function(x, recipe, reprep = TRUE) {
  action <- new_action_recipe(recipe, reprep)
  add_action(x, action, "recipe")
}

# ------------------------------------------------------------------------------

fit.action_recipe <- function(object, director) {

  # TODO - How to correctly handle `reprep = FALSE`? What do we put in the
  # mold slot?

  if (!object$reprep) {
    abort("`reprep = FALSE` is not yet supported.")
    #return(director)
  }

  recipe <- object$recipe
  data <- director$data

  mold <- hardhat::mold(recipe, data)

  new_action <- new_action_recipe(recipe = recipe, reprep = TRUE, mold = mold, done = TRUE)

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

new_action_recipe <- function(recipe, reprep, mold = NULL, done = FALSE) {
  if (!is_recipe(recipe)) {
    abort("`recipe` must be a recipe.")
  }

  vec_assert(reprep, ptype = logical(), size = 1L)

  new_action_pre(recipe = recipe, reprep = reprep, mold = mold, done = done, subclass = "action_recipe")
}

is_recipe <- function(x) {
  inherits(x, "recipe")
}
