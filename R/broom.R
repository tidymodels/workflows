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

  abort("`what` must be 'model' or 'recipe'.", .internal = TRUE)
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
#' if (rlang::is_installed("broom")) {
#'
#' library(parsnip)
#' library(magrittr)
#' library(modeldata)
#'
#' data("attrition")
#'
#' model <- logistic_reg() %>%
#'   set_engine("glm")
#'
#' wf <- workflow() %>%
#'   add_model(model) %>%
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
#' model <- logistic_reg() %>%
#'   set_engine("glm")
#'
#' wf <- workflow() %>%
#'   add_model(model) %>%
#'   add_formula(
#'     Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime
#'   )
#'
#' wf_fit <- fit(wf, attrition)
#'
#' augment(wf_fit, attrition)
#'
#' }
augment.workflow <- function(x, new_data, ...) {
  fit <- extract_fit_parsnip(x)

  # `augment.model_fit()` requires the pre-processed `new_data`
  predictors <- forge_predictors(new_data, x)
  predictors <- prepare_augment_predictors(predictors)
  predictors_and_predictions <- augment(fit, predictors, ...)

  prediction_columns <- setdiff(
    names(predictors_and_predictions),
    names(predictors)
  )

  predictions <- predictors_and_predictions[prediction_columns]

  # Return original `new_data` with new prediction columns
  out <- vctrs::vec_cbind(new_data, predictions)

  out
}

prepare_augment_predictors <- function(x) {
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
    abort("Unknown predictor type returned by `forge_predictors()`.", .internal = TRUE)
  }
}
