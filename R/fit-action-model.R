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
  add_action(x, action)
}

# ------------------------------------------------------------------------------

new_action_model <- function(model, formula) {
  if (!is_model_spec(model)) {
    abort("`model` must be a model_spec.")
  }

  if (!is.null(formula) && !is_formula(formula)) {
    abort("`formula` must be a formula, or `NULL`.")
  }

  new_action_fit(model = model, formula = formula, subclass = "action_model")
}

is_model_spec <- function(x) {
  inherits(x, "model_spec")
}
