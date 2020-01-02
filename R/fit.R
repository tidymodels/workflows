#' Fit a workflow object
#'
#' @description
#' Fitting a workflow currently involves two main steps:
#'
#' - Preprocessing the data using a formula preprocessor, or by calling
#'   [recipes::prep()] on a recipe.
#'
#' - Fitting the underlying parsnip model using [parsnip::fit.model_spec()].
#'
#' @details
#' In the future, there will also be _postprocessing_ steps that can be added
#' after the model has been fit.
#'
#' @param object A workflow
#'
#' @param data A data frame of predictors and outcomes to use when fitting the
#'   workflow
#'
#' @param ... Not used
#'
#' @param control A [control_workflow()] object
#'
#' @return
#' The workflow `object`, updated with a fit parsnip model in the
#' `object$fit$fit` slot.
#'
#' @name fit-workflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' base_workflow <- workflow()
#' base_workflow <- add_model(base_workflow, model)
#'
#' formula_workflow <- add_formula(base_workflow, mpg ~ cyl + log(disp))
#'
#' fit(formula_workflow, mtcars)
#'
#' recipe <- recipe(mpg ~ cyl + disp, mtcars)
#' recipe <- step_log(recipe, disp)
#'
#' recipe_workflow <- add_recipe(base_workflow, recipe)
#'
#' fit(recipe_workflow, mtcars)
fit.workflow <- function(object, data, ..., control = control_workflow()) {
  workflow <- object

  if (is_missing(data)) {
    abort("`data` must be provided to fit a workflow.")
  }

  ellipsis::check_dots_empty()
  validate_has_minimal_components(object)

  workflow <- .fit_pre(workflow, data)
  workflow <- .fit_model(workflow, control)

  # Eh? Predictions during the fit?
  # pred <- result$pred
  # result <- fit_post(workflow, pred)

  workflow$trained <- TRUE

  workflow
}

# ------------------------------------------------------------------------------

#' Internal workflow functions
#'
#' `.fit_pre()` and `.fit_model()` are internal workflow functions for
#' _partially_ fitting a workflow object. They are only exported for usage by
#' the tuning package, [tune](https://github.com/tidymodels/tune), and the
#' general user should never need to worry about them.
#'
#' @param workflow A workflow
#'
#'   For `.fit_pre()`, this should be a fresh workflow.
#'
#'   For `.fit_model()`, this should be a workflow that has already been trained
#'   through `.fit_pre()`.
#'
#' @param data A data frame of predictors and outcomes to use when fitting the
#'   workflow
#'
#' @param control A [control_workflow()] object
#'
#' @name workflows-internals
#' @keywords internal
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#'
#' model <- linear_reg()
#' model <- set_engine(model, "lm")
#'
#' base_workflow <- workflow()
#' base_workflow <- add_model(base_workflow, model)
#'
#' formula_workflow <- add_formula(base_workflow, mpg ~ cyl + log(disp))
#'
#' partially_fit_workflow <- .fit_pre(formula_workflow, mtcars)
#' fit_workflow <- .fit_model(partially_fit_workflow, control_workflow())
.fit_pre <- function(workflow, data) {
  n <- length(workflow[["pre"]]$actions)

  for(i in seq_len(n)) {
    action <- workflow[["pre"]]$actions[[i]]

    # Update both the `workflow` and the `data` as we iterate through pre steps
    result <- fit(action, workflow = workflow, data = data)
    workflow <- result$workflow
    data <- result$data
  }

  # But only return the workflow, it contains the final set of data in `mold`
  workflow
}

#' @rdname workflows-internals
#' @export
.fit_model <- function(workflow, control) {
  action_model <- workflow[["fit"]][["actions"]][["model"]]
  fit(action_model, workflow = workflow, control = control)
}

# ------------------------------------------------------------------------------

validate_has_minimal_components <- function(x) {
  has_preprocessor <- has_action(x$pre, "formula") || has_action(x$pre, "recipe")

  if (!has_preprocessor) {
    glubort(
      "The workflow must have a formula or recipe preprocessor. ",
      "Provide one with `add_formula()` or `add_recipe()`."
    )
  }

  has_model <- has_action(x$fit, "model")

  if (!has_model) {
    glubort(
      "The workflow must have a model. ",
      "Provide one with `add_model()`."
    )
  }

  invisible(x)
}
