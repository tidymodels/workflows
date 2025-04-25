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
#'   and predict on. If using a recipe preprocessor, you should not call
#'   [recipes::bake()] on `new_data` before passing to this function.
#'
#' @return
#' A data frame of model predictions, with as many rows as `new_data` has.
#'
#' @name predict-workflow
#' @export
#' @examplesIf rlang::is_installed("recipes")
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' training <- mtcars[1:20, ]
#' testing <- mtcars[21:32, ]
#'
#' model <- linear_reg() |>
#'   set_engine("lm")
#'
#' workflow <- workflow() |>
#'   add_model(model)
#'
#' recipe <- recipe(mpg ~ cyl + disp, training) |>
#'   step_log(disp)
#'
#' workflow <- add_recipe(workflow, recipe)
#'
#' fit_workflow <- fit(workflow, training)
#'
#' # This will automatically `bake()` the recipe on `testing`,
#' # applying the log step to `disp`, and then fit the regression.
#' predict(fit_workflow, testing)
predict.workflow <- function(
  object,
  new_data,
  type = NULL,
  opts = list(),
  ...
) {
  workflow <- object

  if (!is_trained_workflow(workflow)) {
    cli_abort(c(
      "Can't predict on an untrained workflow.",
      "i" = "Do you need to call {.fun fit}?"
    ))
  }

  if (is_sparse_matrix(new_data)) {
    new_data <- sparsevctrs::coerce_to_sparse_tibble(
      new_data,
      call = rlang::caller_env(0)
    )
  }

  fit <- extract_fit_parsnip(workflow)
  new_data <- forge_predictors(new_data, workflow)

  if (!has_postprocessor(workflow)) {
    return(predict(fit, new_data, type = type, opts = opts, ...))
  }

  # use `augment()` rather than `fit()` to get all possible prediction `type`s (#234).
  fit_aug <- augment(fit, new_data, opts = opts, ...)

  post <- extract_postprocessor(workflow)
  predict(post, fit_aug)[predict_type_column_names(type, post$columns)]
}

forge_predictors <- function(new_data, workflow) {
  mold <- extract_mold(workflow)
  forged <- hardhat::forge(new_data, blueprint = mold$blueprint)
  forged$predictors
}

predict_type_column_names <- function(
  type,
  tailor_columns,
  call = caller_env()
) {
  check_string(type, allow_null = TRUE, call = call)

  if (is.null(type)) {
    return(tailor_columns$estimate)
  }

  switch(
    type,
    numeric = ,
    class = tailor_columns$estimate,
    prob = tailor_columns$probabilities,
    cli::cli_abort(
      "Unsupported prediction {.arg type} {.val {type}} for a workflow with a postprocessor.",
      call = call
    )
  )
}
