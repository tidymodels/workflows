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
#' @includeRmd man/rmd/indicators.Rmd details
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
#' library(magrittr)
#'
#' model <- linear_reg() %>%
#'   set_engine("lm")
#'
#' base_wf <- workflow() %>%
#'   add_model(model)
#'
#' formula_wf <- base_wf %>%
#'   add_formula(mpg ~ cyl + log(disp))
#'
#' fit(formula_wf, mtcars)
#'
#' recipe <- recipe(mpg ~ cyl + disp, mtcars) %>%
#'   step_log(disp)
#'
#' recipe_wf <- base_wf %>%
#'   add_recipe(recipe)
#'
#' fit(recipe_wf, mtcars)
fit.workflow <- function(object, data, ..., control = control_workflow()) {
  ellipsis::check_dots_empty()

  if (is_missing(data)) {
    abort("`data` must be provided to fit a workflow.")
  }

  workflow <- object
  workflow <- .fit_pre(workflow, data)
  workflow <- .fit_model(workflow, control)
  workflow <- .fit_finalize(workflow)

  # TODO: Post-processing before `.fit_finalize()`?

  workflow
}

# ------------------------------------------------------------------------------

#' Internal workflow functions
#'
#' `.fit_pre()`, `.fit_model()`, and `.fit_finalize()` are internal workflow
#' functions for _partially_ fitting a workflow object. They are only exported
#' for usage by the tuning package, [tune](https://github.com/tidymodels/tune),
#' and the general user should never need to worry about them.
#'
#' @param workflow A workflow
#'
#'   For `.fit_pre()`, this should be a fresh workflow.
#'
#'   For `.fit_model()`, this should be a workflow that has already been trained
#'   through `.fit_pre()`.
#'
#'   For `.fit_finalize()`, this should be a workflow that has been through
#'   both `.fit_pre()` and `.fit_model()`.
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
#' library(magrittr)
#'
#' model <- linear_reg() %>%
#'   set_engine("lm")
#'
#' wf_unfit <- workflow() %>%
#'   add_model(model) %>%
#'   add_formula(mpg ~ cyl + log(disp))
#'
#' wf_fit_pre <- .fit_pre(wf_unfit, mtcars)
#' wf_fit_model <- .fit_model(wf_fit_pre, control_workflow())
#' wf_fit <- .fit_finalize(wf_fit_model)
#'
#' # Notice that fitting through the model doesn't mark the
#' # workflow as being "trained"
#' wf_fit_model
#'
#' # Finalizing the workflow marks it as "trained"
#' wf_fit
#'
#' # Which allows you to predict from it
#' try(predict(wf_fit_model, mtcars))
#'
#' predict(wf_fit, mtcars)
.fit_pre <- function(workflow, data) {
  validate_has_preprocessor(workflow)
  # A model spec is required to ensure that we can always
  # finalize the blueprint, no matter the preprocessor
  validate_has_model(workflow)

  workflow <- finalize_blueprint(workflow)

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

#' @rdname workflows-internals
#' @export
.fit_finalize <- function(workflow) {
  workflow[["trained"]] <- TRUE
  workflow
}

# ------------------------------------------------------------------------------

validate_has_preprocessor <- function(x) {
  has_preprocessor <-
    has_preprocessor_formula(x) ||
    has_preprocessor_recipe(x) ||
    has_preprocessor_variables(x)

  if (!has_preprocessor) {
    glubort(
      "The workflow must have formula, recipe, or variables preprocessor. ",
      "Provide one with `add_formula()`, `add_recipe()`, or `add_variables()`."
    )
  }

  invisible(x)
}

validate_has_model <- function(x) {
  has_model <- has_action(x$fit, "model")

  if (!has_model) {
    glubort(
      "The workflow must have a model. ",
      "Provide one with `add_model()`."
    )
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

finalize_blueprint <- function(workflow) {
  # Use user supplied blueprint if provided
  if (has_blueprint(workflow)) {
    return(workflow)
  }

  if (has_preprocessor_recipe(workflow)) {
    finalize_blueprint_recipe(workflow)
  } else if (has_preprocessor_formula(workflow)) {
    finalize_blueprint_formula(workflow)
  } else if (has_preprocessor_variables(workflow)) {
    finalize_blueprint_variables(workflow)
  } else {
    abort("Internal error: `workflow` should have a preprocessor at this point.")
  }
}

finalize_blueprint_recipe <- function(workflow) {
  # Use the default blueprint, no parsnip model encoding info is used here
  blueprint <- hardhat::default_recipe_blueprint()

  recipe <- pull_workflow_preprocessor(workflow)

  update_recipe(workflow, recipe = recipe, blueprint = blueprint)
}

finalize_blueprint_formula <- function(workflow) {
  tbl_encodings <- pull_workflow_spec_encoding_tbl(workflow)

  indicators <- tbl_encodings$predictor_indicators
  intercept <- tbl_encodings$compute_intercept

  if (!is_string(indicators)) {
    abort("Internal error: `indicators` encoding from parsnip should be a string.")
  }
  if (!is_bool(intercept)) {
    abort("Internal error: `intercept` encoding from parsnip should be a bool.")
  }

  # Use model specific information to construct the blueprint
  blueprint <- hardhat::default_formula_blueprint(
    indicators = indicators,
    intercept = intercept
  )

  formula <- pull_workflow_preprocessor(workflow)

  update_formula(workflow, formula = formula, blueprint = blueprint)
}

pull_workflow_spec_encoding_tbl <- function(workflow) {
  spec <- pull_workflow_spec(workflow)
  spec_cls <- class(spec)[[1]]

  tbl_encodings <- parsnip::get_encoding(spec_cls)

  indicator_engine <- tbl_encodings$engine == spec$engine
  indicator_mode <- tbl_encodings$mode == spec$mode
  indicator_spec <- indicator_engine & indicator_mode

  out <- tbl_encodings[indicator_spec, , drop = FALSE]

  if (nrow(out) != 1L) {
    abort("Internal error: Exactly 1 model/engine/mode combination must be located.")
  }

  out
}

finalize_blueprint_variables <- function(workflow) {
  # Use the default blueprint, no parsnip model encoding info is used here
  blueprint <- hardhat::default_xy_blueprint()

  variables <- pull_workflow_preprocessor(workflow)

  update_variables(
    workflow,
    outcomes = !!variables$outcomes,
    predictors = !!variables$predictors,
    blueprint = blueprint
  )
}
