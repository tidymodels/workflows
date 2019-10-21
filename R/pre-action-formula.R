#' @export
add_formula <- function(x, formula) {
  action <- new_action_formula(formula)
  add_action(x, action, "formula")
}

# ------------------------------------------------------------------------------

fit.action_formula <- function(object, workflow, data) {
  formula <- object$formula

  mold <- hardhat::mold(formula, data)

  # TODO - Strip out formula environment at some point?
  new_action <- new_action_formula(formula = formula, mold = mold)

  workflow$pre$actions$formula <- new_action

  # TODO - does `data` need to be returned?
  list(workflow = workflow, data = data)
}

# ------------------------------------------------------------------------------

check_conflicts.action_formula <- function(action, x) {
  pre <- x$pre

  if (has_action(pre, "recipe")) {
    abort("A formula cannot be added when a recipe already exists.")
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_formula <- function(formula, mold = NULL) {
  if (!is_formula(formula)) {
    abort("`formula` must be a formula.")
  }

  new_action_pre(formula = formula, mold = mold, subclass = "action_formula")
}
