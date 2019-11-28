#' Add formula terms to a workflow
#'
#' `add_formula()` specifies the terms of the model through the usage of a
#' formula.
#'
#' `remove_formula()` removes the formula as well as any objects for the
#'  application of the formula to the data (e.g., terms, if they exist), as well
#'  as any fitted model that used the formula.
#'
#' `update_formula()` first removes the formula, then replaces the previous
#'  formula with the new one. Any fitted model based on this formula will need
#'  to be refit.
#'
#' To fit a workflow, one of `add_formula()` or `add_recipe()` _must_ be
#' specified, but not both.
#'
#' @param x A workflow
#'
#' @param formula A formula specifying the terms of the model. It is advised to
#' not do preprocessing in the formula, and instead use a recipe if that is
#' required.
#'
#' @export
#' @examples
#' workflow <- workflow()
#' workflow <- add_formula(workflow, mpg ~ cyl)
#' workflow
#'
#' remove_formula(workflow)
#'
#' update_formula(workflow, mpg ~ disp)
add_formula <- function(x, formula) {
  action <- new_action_formula(formula)
  add_action(x, action, "formula")
}

#' @rdname add_formula
#' @export
remove_formula <- function(x) {
  check_existing_object(x, "formula")
  new_workflow(
    pre = new_stage_pre(),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_formula
#' @export
update_formula <- function(x, formula) {
  x <- remove_formula(x)
  add_formula(x, formula)
}

# ------------------------------------------------------------------------------

fit.action_formula <- function(object, workflow, data) {
  formula <- object$formula

  # TODO - Strip out the formula environment at some time?
  workflow$pre$mold <- hardhat::mold(formula, data)

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

new_action_formula <- function(formula) {
  if (!is_formula(formula)) {
    abort("`formula` must be a formula.")
  }

  new_action_pre(formula = formula, subclass = "action_formula")
}
