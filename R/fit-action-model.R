#' Add a model to a workflow
#'
#' `add_model()` adds a parsnip model to the workflow.
#'
#' `add_model()` is a required step to construct a minimal workflow.
#'
#' @param x A workflow.
#'
#' @param model A parsnip model.
#'
#' @param formula An optional formula override to specify the terms of the
#'   model. Typically, the terms are extracted from the formula or recipe
#'   preprocessing methods. However, some models (like survival and bayesian
#'   models) use the formula not to preprocess, but to specify the structure
#'   of the model. In those cases, a formula specifying the model structure
#'   must be passed unchanged into the model call itself. This argument is
#'   used for those purposes.
#'
#' @export
add_model <- function(x, model, formula = NULL) {
  action <- new_action_model(model, formula)
  add_action(x, action, "model")
}

# ------------------------------------------------------------------------------

fit.action_model <- function(object, workflow, data, ctrl) {
  ctrl_parsnip <- ctrl$parsnip

  model <- object$model
  formula <- object$formula

  mold <- pull_mold(workflow)

  if (is.null(formula)) {
    fit_model <- fit_from_xy(model, mold, ctrl_parsnip)
  } else {
    fit_model <- fit_from_formula(model, mold, ctrl_parsnip, formula)
  }

  new_action <- new_action_model(fit_model, formula)

  workflow$fit$actions$model <- new_action

  # TODO - does `data` need to be returned?
  list(workflow = workflow, data = data)
}

fit_from_xy <- function(model, mold, ctrl_parsnip) {
  fit_xy(model, x = mold$predictors, y = mold$outcomes, control = ctrl_parsnip)
}

fit_from_formula <- function(model, mold, ctrl_parsnip, formula) {
  data <- vec_cbind(mold$outcomes, mold$predictors)
  fit(model, formula = formula, data = data, control = ctrl_parsnip)
}

pull_mold <- function(workflow) {
  pre <- workflow$pre

  if (has_action(pre, "formula")) {
    pre$actions$formula$mold
  } else if (has_action(pre, "recipe")) {
    pre$actions$recipe$mold
  } else {
    abort("Internal error: No mold exists. `workflow` pre stage has not been run.")
  }
}

# ------------------------------------------------------------------------------

new_action_model <- function(model, formula) {
  if (!is_model_spec_or_fit(model)) {
    abort("`model` must be a `model_spec` or `model_fit`.")
  }

  if (!is.null(formula) && !is_formula(formula)) {
    abort("`formula` must be a formula, or `NULL`.")
  }

  new_action_fit(model = model, formula = formula, subclass = "action_model")
}

is_model_spec_or_fit <- function(x) {
  inherits(x, "model_spec") || inherits(x, "model_fit")
}
