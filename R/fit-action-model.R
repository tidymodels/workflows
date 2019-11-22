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
#' @examples
#' library(parsnip)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' workflow <- workflow()
#' workflow <- add_model(workflow, model)
add_model <- function(x, spec, formula = NULL) {
  action <- new_action_model(spec, formula)
  add_action(x, action, "model")
}

# ------------------------------------------------------------------------------

fit.action_model <- function(object, workflow, control) {
  control_parsnip <- control$control_parsnip

  spec <- object$spec
  formula <- object$formula

  mold <- workflow$pre$mold

  if (is.null(mold)) {
    abort("Internal error: No mold exists. `workflow` pre stage has not been run.")
  }

  if (is.null(formula)) {
    fit <- fit_from_xy(spec, mold, control_parsnip)
  } else {
    fit <- fit_from_formula(spec, mold, control_parsnip, formula)
  }

  workflow$fit$fit <- fit

  # Only the workflow is returned
  workflow
}

fit_from_xy <- function(spec, mold, control_parsnip) {
  fit_xy(spec, x = mold$predictors, y = mold$outcomes, control = control_parsnip)
}

fit_from_formula <- function(spec, mold, control_parsnip, formula) {
  data <- vec_cbind(mold$outcomes, mold$predictors)
  fit(spec, formula = formula, data = data, control = control_parsnip)
}

# ------------------------------------------------------------------------------

new_action_model <- function(spec, formula) {
  if (!is_model_spec(spec)) {
    abort("`spec` must be a `model_spec`.")
  }

  if (!is.null(formula) && !is_formula(formula)) {
    abort("`formula` must be a formula, or `NULL`.")
  }

  new_action_fit(spec = spec, formula = formula, subclass = "action_model")
}
