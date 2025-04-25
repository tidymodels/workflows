#' Tidy a workflow
#'
#' @description
#' This is a [generics::tidy()] method for a workflow that calls `tidy()` on
#' either the underlying parsnip model or the recipe, depending on the value
#' of `what`.
#'
#' `x` must be a fitted workflow, resulting in fitted parsnip model or prepped
#' recipe that you want to tidy.
#'
#' @details
#' To tidy the unprepped recipe, use [extract_preprocessor()] and `tidy()`
#' that directly.
#'
#' @param x A workflow
#'
#' @param what A single string. Either `"model"` or `"recipe"` to select
#'   which part of the workflow to tidy. Defaults to tidying the model.
#'
#' @param ... Arguments passed on to methods
#'
#' @export
tidy.workflow <- function(x, what = "model", ...) {
  what <- arg_match(what, values = c("model", "recipe"))

  if (identical(what, "model")) {
    x <- extract_fit_parsnip(x)
    out <- tidy(x, ...)
    return(out)
  }

  if (identical(what, "recipe")) {
    x <- extract_recipe(x)
    out <- tidy(x, ...)
    return(out)
  }

  cli_abort(
    "{.arg what} must be {.val model} or {.val recipe}.",
    .internal = TRUE
  )
}

# ------------------------------------------------------------------------------

#' Glance at a workflow model
#'
#' @description
#' This is a [generics::glance()] method for a workflow that calls `glance()` on
#' the underlying parsnip model.
#'
#' `x` must be a trained workflow, resulting in fitted parsnip model to
#' `glance()` at.
#'
#' @param x A workflow
#'
#' @param ... Arguments passed on to methods
#'
#' @export
#' @examples
#' if (rlang::is_installed(c("broom", "modeldata"))) {
#'
#' library(parsnip)
#' library(magrittr)
#' library(modeldata)
#'
#' data("attrition")
#'
#' model <- logistic_reg() |>
#'   set_engine("glm")
#'
#' wf <- workflow() |>
#'   add_model(model) |>
#'   add_formula(
#'     Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime
#'   )
#'
#' # Workflow must be trained to call `glance()`
#' try(glance(wf))
#'
#' wf_fit <- fit(wf, attrition)
#'
#' glance(wf_fit)
#'
#' }
glance.workflow <- function(x, ...) {
  x <- extract_fit_parsnip(x)
  glance(x, ...)
}

# ------------------------------------------------------------------------------

#' Augment data with predictions
#'
#' @description
#' This is a [generics::augment()] method for a workflow that calls
#' `augment()` on the underlying parsnip model with `new_data`.
#'
#' `x` must be a trained workflow, resulting in fitted parsnip model to
#' `augment()` with.
#'
#' `new_data` will be preprocessed using the preprocessor in the workflow,
#' and that preprocessed data will be used to generate predictions. The
#' final result will contain the original `new_data` with new columns containing
#' the prediction information.
#'
#' @param x A workflow
#'
#' @param new_data A data frame of predictors
#'
#' @param ... Arguments passed on to methods
#'
#' @return `new_data` with new prediction specific columns.
#'
#' @param eval_time For censored regression models, a vector of time points at
#' which the survival probability is estimated. See
#' [parsnip::augment.model_fit()] for more details.
#'
#' @export
#' @examples
#' if (rlang::is_installed("broom")) {
#'
#' library(parsnip)
#' library(magrittr)
#' library(modeldata)
#'
#' data("attrition")
#'
#' model <- logistic_reg() |>
#'   set_engine("glm")
#'
#' wf <- workflow() |>
#'   add_model(model) |>
#'   add_formula(
#'     Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime
#'   )
#'
#' wf_fit <- fit(wf, attrition)
#'
#' augment(wf_fit, attrition)
#'
#' }
augment.workflow <- function(x, new_data, eval_time = NULL, ...) {
  fit <- extract_fit_parsnip(x)
  mold <- extract_mold(x)

  # supply outcomes to `augment.model_fit()` if possible (#131)
  outcomes <- FALSE
  if (length(fit$preproc$y_var) > 0) {
    outcomes <- all(fit$preproc$y_var %in% names(new_data))
  }

  # `augment.model_fit()` requires the pre-processed `new_data`
  forged <- hardhat::forge(
    new_data,
    blueprint = mold$blueprint,
    outcomes = outcomes
  )

  if (outcomes) {
    new_data_forged <- vctrs::vec_cbind(forged$predictors, forged$outcomes)
  } else {
    new_data_forged <- forged$predictors
  }

  new_data_forged <- prepare_augment_new_data(new_data_forged)
  out <- augment(fit, new_data_forged, eval_time = eval_time, ...)

  if (has_postprocessor_tailor(x)) {
    post <- extract_postprocessor(x)
    out <- predict(post, new_data = out)
  }

  augment_columns <- setdiff(
    names(out),
    names(new_data_forged)
  )

  out <- out[augment_columns]

  # Return original `new_data` with new prediction columns
  out <- vctrs::vec_cbind(out, new_data)

  out
}

prepare_augment_new_data <- function(x) {
  # `augment()` works best with a data frame of predictors,
  # so we need to undo any matrix/sparse matrix compositions that
  # were returned from `hardhat::forge()` (#148)
  if (is.data.frame(x)) {
    x
  } else if (is.matrix(x)) {
    as.data.frame(x)
  } else if (inherits(x, "dgCMatrix")) {
    x <- as.matrix(x)
    as.data.frame(x)
  } else {
    cli_abort(
      "Unknown predictor type returned by {.fun forge_predictors}.",
      .internal = TRUE
    )
  }
}
