#' Add a model to a workflow
#'
#' @description
#' - `add_model()` adds a parsnip model to the workflow.
#'
#' - `remove_model()` removes the model specification as well as any fitted
#'   model object. Any extra formulas are also removed.
#'
#' - `update_model()` first removes the model then adds the new specification to
#'   the workflow.
#'
#' @details
#' `add_model()` is a required step to construct a minimal workflow.
#'
#' @includeRmd man/rmd/indicators.Rmd details
#'
#' @inheritParams ellipsis::dots_empty
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
#' @return
#' `x`, updated with either a new or removed model.
#'
#' @export
#' @examples
#' library(parsnip)
#'
#' lm_model <- linear_reg()
#' lm_model <- set_engine(lm_model, "lm")
#'
#' regularized_model <- set_engine(lm_model, "glmnet")
#'
#' workflow <- workflow()
#' workflow <- add_model(workflow, lm_model)
#' workflow
#'
#' workflow <- add_formula(workflow, mpg ~ .)
#' workflow
#'
#' remove_model(workflow)
#'
#' fitted <- fit(workflow, data = mtcars)
#' fitted
#'
#' remove_model(fitted)
#'
#' remove_model(workflow)
#'
#' update_model(workflow, regularized_model)
#' update_model(fitted, regularized_model)
#'
add_model <- function(x, spec, ..., formula = NULL) {
  ellipsis::check_dots_empty()
  action <- new_action_model(spec, formula)
  add_action(x, action, "model")
}

#' @rdname add_model
#' @export
remove_model <- function(x) {
  validate_is_workflow(x)

  if (!has_spec(x)) {
    rlang::warn("The workflow has no model to remove.")
  }

  new_workflow(
    pre = x$pre,
    fit = new_stage_fit(),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}


#' @rdname add_model
#' @export
update_model <- function(x, spec, ..., formula = NULL) {
  ellipsis::check_dots_empty()
  x <- remove_model(x)
  add_model(x, spec, formula = formula)
}

# ------------------------------------------------------------------------------

fit.action_model <- function(object, workflow, control) {
  if (!is_control_workflow(control)) {
    abort("`control` must be a workflows control object created by `control_workflow()`.")
  }

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
  data <- cbind(mold$outcomes, mold$predictors)
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
