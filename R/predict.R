#' Predict from a workflow
#'
#' @description
#' This is the `predict()` method for a fit workflow object. The nice thing
#' about predicting from a workflow is that it will:
#'
#' - Preprocess `new_data` using the preprocessing method specified when the
#'   workflow was created and fit. This is accomplished using
#'   [hardhat::forge()], which will apply any formula preprocessing or call
#'   [recipes::bake()] if a recipe was supplied.
#'
#' - Call [parsnip::predict.model_fit()] for you using the underlying fit
#'   parsnip model.
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @param object A workflow that has been fit by [fit.workflow()]
#'
#' @param new_data A data frame containing the new predictors to preprocess
#'   and predict on
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has.
#'
#' @name predict-workflow
#' @export
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' training <- mtcars[1:20, ]
#' testing <- mtcars[21:32, ]
#'
#' model <- linear_reg() %>%
#'   set_engine("lm")
#'
#' workflow <- workflow() %>%
#'   add_model(model)
#'
#' recipe <- recipe(mpg ~ cyl + disp, training) %>%
#'   step_log(disp)
#'
#' workflow <- add_recipe(workflow, recipe)
#'
#' fit_workflow <- fit(workflow, training)
#'
#' # This will automatically `bake()` the recipe on `testing`,
#' # applying the log step to `disp`, and then fit the regression.
#' predict(fit_workflow, testing)
predict.workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  workflow <- object

  if (!is_trained_workflow(workflow)) {
    abort(c(
      "Can't predict on an untrained workflow.",
      i = "Do you need to call `fit()`?"
    ))
  }

  fit <- extract_fit_parsnip(workflow)
  new_data <- forge_predictors(new_data, workflow)

  predict(fit, new_data, type = type, opts = opts, ...)
}

forge_predictors <- function(new_data, workflow) {
  mold <- extract_mold(workflow)
  forged <- hardhat::forge(new_data, blueprint = mold$blueprint)
  forged$predictors
}
