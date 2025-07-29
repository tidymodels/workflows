#' Fit a workflow object
#'
#' @description
#' Fitting a workflow currently involves three main steps:
#'
#' - Preprocessing the data using a formula preprocessor, or by calling
#'   [recipes::prep()] on a recipe.
#'
#' - Fitting the underlying parsnip model using [parsnip::fit.model_spec()].
#'
#' - Postprocessing predictions from the model using
#'   [tailor::tailor()].
#'
#' @includeRmd man/rmd/indicators.Rmd details
#'
#' @param object A workflow
#'
#' @param data A data frame of predictors and outcomes to use when fitting the
#'   preprocessor and model.
#'
#' @param ... Not used
#'
#' @param calibration A data frame of predictors and outcomes to use when
#'   fitting the postprocessor. See the "Data Usage" section of [add_tailor()]
#'   for more information.
#'
#' @param control A [control_workflow()] object
#'
#' @return
#' The workflow `object`, updated with a fit parsnip model in the
#' `object$fit$fit` slot.
#'
#' @name fit-workflow
#' @export
#' @examplesIf rlang::is_installed("recipes")
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' model <- linear_reg() |>
#'   set_engine("lm")
#'
#' base_wf <- workflow() |>
#'   add_model(model)
#'
#' formula_wf <- base_wf |>
#'   add_formula(mpg ~ cyl + log(disp))
#'
#' fit(formula_wf, mtcars)
#'
#' recipe <- recipe(mpg ~ cyl + disp, mtcars) |>
#'   step_log(disp)
#'
#' recipe_wf <- base_wf |>
#'   add_recipe(recipe)
#'
#' fit(recipe_wf, mtcars)
fit.workflow <- function(
  object,
  data,
  ...,
  calibration = NULL,
  control = control_workflow()
) {
  check_dots_empty()

  if (is_missing(data)) {
    cli_abort("{.arg data} must be provided to fit a workflow.")
  }

  validate_has_calibration(object, calibration)

  if (is_sparse_matrix(data)) {
    data <- sparsevctrs::coerce_to_sparse_tibble(
      data,
      call = rlang::caller_env(0)
    )
  }

  object <- toggle_sparsity(object, data)

  workflow <- object
  workflow <- .fit_pre(workflow, data)
  workflow <- .fit_model(workflow, control)

  if (!.workflow_includes_calibration(workflow)) {
    # in this case, training the tailor on `data` will not leak data (#262)
    calibration <- data
  }
  if (has_postprocessor(workflow)) {
    workflow <- .fit_post(workflow, calibration)
  }

  workflow <- .fit_finalize(workflow)

  workflow
}

#' @export
#' @rdname workflows-internals
#' @keywords internal
.workflow_includes_calibration <- function(workflow) {
  has_postprocessor(workflow) &&
    tailor::tailor_requires_fit(
      extract_postprocessor(workflow, estimated = FALSE)
    )
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
#' @examplesIf rlang::is_installed("recipes")
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' model <- linear_reg() |>
#'   set_engine("lm")
#'
#' wf_unfit <- workflow() |>
#'   add_model(model) |>
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

  for (i in seq_len(n)) {
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
.fit_post <- function(workflow, data) {
  action_post <- workflow[["post"]][["actions"]][["tailor"]]
  fit(action_post, workflow = workflow, data = data)
}

#' @rdname workflows-internals
#' @export
.fit_finalize <- function(workflow) {
  set_trained(workflow, TRUE)
}

# ------------------------------------------------------------------------------

validate_has_preprocessor <- function(x, ..., call = caller_env()) {
  check_dots_empty()

  has_preprocessor <-
    has_preprocessor_formula(x) ||
    has_preprocessor_recipe(x) ||
    has_preprocessor_variables(x)

  if (!has_preprocessor) {
    message <- c(
      "The workflow must have a formula, recipe, or variables preprocessor.",
      i = "Provide one with {.fun add_formula}, {.fun add_recipe},
           or {.fun add_variables}."
    )
    cli_abort(message, call = call)
  }

  invisible(x)
}

validate_has_model <- function(x, ..., call = caller_env()) {
  check_dots_empty()

  has_model <- has_action(x$fit, "model")

  if (!has_model) {
    message <- c(
      "The workflow must have a model.",
      i = "Provide one with {.fun add_model}."
    )
    cli_abort(message, call = call)
  }

  invisible(x)
}

validate_has_calibration <- function(x, calibration, call = caller_env()) {
  if (.workflow_includes_calibration(x) && is.null(calibration)) {
    cli::cli_abort(
      "The workflow requires a {.arg calibration} set to train but none
       was supplied.",
      call = call
    )
  }

  if (!.workflow_includes_calibration(x) && !is.null(calibration)) {
    cli::cli_warn(
      "The workflow does not require a {.arg calibration} set to train
       but one was supplied.",
      call = call
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
    cli_abort(
      "{.arg workflow} should have a preprocessor at this point.",
      .internal = TRUE
    )
  }
}

finalize_blueprint_recipe <- function(workflow) {
  # Use the default blueprint, no parsnip model encoding info is used here
  blueprint <- hardhat::default_recipe_blueprint()

  recipe <- extract_preprocessor(workflow)

  update_recipe(workflow, recipe = recipe, blueprint = blueprint)
}

finalize_blueprint_formula <- function(workflow) {
  tbl_encodings <- pull_workflow_spec_encoding_tbl(workflow)

  indicators <- tbl_encodings$predictor_indicators
  intercept <- tbl_encodings$compute_intercept

  if (!is_string(indicators)) {
    cli_abort(
      "`indicators` encoding from parsnip should be a string.",
      .internal = TRUE
    )
  }
  if (!is_bool(intercept)) {
    cli_abort(
      "`intercept` encoding from parsnip should be a bool.",
      .internal = TRUE
    )
  }

  # Use model specific information to construct the blueprint
  blueprint <- hardhat::default_formula_blueprint(
    indicators = indicators,
    intercept = intercept
  )

  formula <- extract_preprocessor(workflow)

  update_formula(workflow, formula = formula, blueprint = blueprint)
}

pull_workflow_spec_encoding_tbl <- function(workflow) {
  spec <- extract_spec_parsnip(workflow)
  spec_cls <- class(spec)[[1]]

  if (modelenv::is_unsupervised_spec(spec)) {
    tbl_encodings <- modelenv::get_encoding(spec_cls)
  } else {
    tbl_encodings <- parsnip::get_encoding(spec_cls)
  }

  indicator_engine <- tbl_encodings$engine == spec$engine
  indicator_mode <- tbl_encodings$mode == spec$mode
  indicator_spec <- indicator_engine & indicator_mode

  out <- tbl_encodings[indicator_spec, , drop = FALSE]

  if (nrow(out) != 1L) {
    cli_abort(
      "Exactly 1 model/engine/mode combination must be located.",
      .internal = TRUE
    )
  }

  out
}

finalize_blueprint_variables <- function(workflow) {
  # Use the default blueprint, no parsnip model encoding info is used here
  blueprint <- hardhat::default_xy_blueprint()

  variables <- extract_preprocessor(workflow)

  update_variables(
    workflow,
    blueprint = blueprint,
    variables = variables
  )
}
