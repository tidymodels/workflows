#' Add formula terms to a workflow
#'
#' `add_formula()` specifies the terms of the model through the usage of a
#' formula.
#'
#' To fit a workflow, one of `add_formula()` or `add_recipe()` _must_ be
#' specified, but not both.
#'
#' @param x A workflow.
#'
#' @param formula A formula specifying the terms of the model. It is advised to
#' not do preprocessing in the formula, and instead use a recipe if that is
#' required.
#'
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

  # All pre steps return the `workflow` and `data`
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
