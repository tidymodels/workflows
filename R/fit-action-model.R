#' Add a model to a workflow
#'
#' `add_model()` adds a parsnip model to the workflow.
#'
#' `add_model()` is a required step to construct a minimal workflow.
#'
#' @param x A workflow.
#'
#' @param spec A parsnip model specification.
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
add_model <- function(x, spec, formula = NULL) {
  action <- new_action_model(spec, formula)
  add_action(x, action, "model")
}

# ------------------------------------------------------------------------------

fit.action_model <- function(object, workflow, ctrl) {
  ctrl_parsnip <- ctrl$ctrl_parsnip

  spec <- object$spec
  formula <- object$formula

  mold <- pull_mold(workflow)

  if (is.null(formula)) {
    fit <- fit_from_xy(spec, mold, ctrl_parsnip)
  } else {
    fit <- fit_from_formula(spec, mold, ctrl_parsnip, formula)
  }

  new_action <- new_action_model(spec = spec, formula = formula, fit = fit)

  workflow$fit$actions$model <- new_action

  # Only the workflow is returned
  workflow
}

fit_from_xy <- function(spec, mold, ctrl_parsnip) {
  fit_xy(spec, x = mold$predictors, y = mold$outcomes, control = ctrl_parsnip)
}

fit_from_formula <- function(spec, mold, ctrl_parsnip, formula) {
  data <- vec_cbind(mold$outcomes, mold$predictors)
  fit(spec, formula = formula, data = data, control = ctrl_parsnip)
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

new_action_model <- function(spec, formula, fit = NULL) {
  if (!is_model_spec(spec)) {
    abort("`spec` must be a `model_spec`.")
  }

  if (!is.null(formula) && !is_formula(formula)) {
    abort("`formula` must be a formula, or `NULL`.")
  }

  if (!is.null(fit) && !is_model_fit(fit)) {
    abort("`fit` must be a `model_fit`.")
  }

  new_action_fit(spec = spec, formula = formula, fit = fit, subclass = "action_model")
}

is_model_spec <- function(x) {
  inherits(x, "model_spec")
}

is_model_fit <- function(x) {
  inherits(x, "model_fit")
}
