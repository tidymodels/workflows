#' Add a tailor to a workflow
#'
#' @description
#' - `add_tailor()` specifies post-processing steps to apply through the
#'   usage of a tailor.
#'
#' - `remove_tailor()` removes the tailor as well as any downstream objects
#'   that might get created after the tailor is used for post-processing, such as
#'   the fitted tailor.
#'
#' - `update_tailor()` first removes the tailor, then replaces the previous
#'   tailor with the new one.
#'
#' @param x A workflow
#'
#' @param tailor A tailor created using [tailor::tailor()]. The tailor
#'   should not have been trained already with [tailor::fit()]; workflows
#'   will handle training internally.
#'
#' @section Data Usage:
#'
#' While preprocessors and models are trained on data in the usual sense,
#' postprocessors are training on _predictions_ on data. When a workflow
#' is fitted, the user typically supplies training data with the `data` argument.
#' When workflows don't contain a postprocessor that requires training,
#' users can pass all of the available data to the `data` argument to train the
#' preprocessor and model. However, in the case where a postprocessor must be
#' trained as well, allotting all of the available data to the `data` argument
#' to train the preprocessor and model would leave no data
#' to train the postprocessor with---if that were the case, workflows
#' would need to `predict()` from the preprocessor and model on the same `data`
#' that they were trained on, with the postprocessor then training on those
#' predictions. Predictions on data that a model was trained on likely follow
#' different distributions than predictions on unseen data; thus, workflows must
#' split up the supplied `data` into two training sets, where the first is used to
#' train the preprocessor and model and the second, called the "calibration set,"
#' is passed to that trained postprocessor and model to generate predictions,
#' which then form the training data for the postprocessor.
#'
#' When fitting a workflow with a postprocessor that requires training
#' (i.e. one that returns `TRUE` in `.workflow_includes_calibration(workflow)`), users
#' must pass two data arguments--the usual `fit.workflow(data)` will be used
#' to train the preprocessor and model while `fit.workflow(calibration)` will
#' be used to train the postprocessor.
#'
#' In some situations, randomly splitting `fit.workflow(data)` (with
#' `rsample::initial_split()`, for example) is sufficient to prevent data
#' leakage. However, `fit.workflow(data)` could also have arisen as:
#'
#' ```
#' boots <- rsample::bootstraps(some_other_data)
#' split <- rsample::get_rsplit(boots, 1)
#' data <- rsample::analysis(split)
#' ```
#'
#' In this case, some of the rows in `data` will be duplicated. Thus, randomly
#' allotting some of them to train the preprocessor and model and others to train
#' the preprocessor would likely result in the same rows appearing in both
#' datasets, resulting in the preprocessor and model generating predictions on
#' rows they've seen before. Similarly problematic situations could arise in the
#' context of other resampling situations, like time-based splits.
#' In general, use the `rsample::inner_split()` function to prevent data
#' leakage when resampling; when workflows with postprocessors that require
#' training are passed to the tune package, this is handled internally.
#'
#' @param ... Not used.
#'
#' @return
#' `x`, updated with either a new or removed tailor postprocessor.
#'
#' @export
#' @examplesIf rlang::is_installed(c("tailor", "probably"))
#' library(tailor)
#' library(magrittr)
#'
#' tailor <- tailor()
#' tailor_1 <- adjust_probability_threshold(tailor, .1)
#'
#' workflow <- workflow() |>
#'   add_tailor(tailor_1)
#'
#' workflow
#'
#' remove_tailor(workflow)
#'
#' update_tailor(workflow, adjust_probability_threshold(tailor, .2))
add_tailor <- function(x, tailor, ...) {
  check_dots_empty()
  validate_tailor_available()
  action <- new_action_tailor(tailor)
  res <- add_action(x, action, "tailor")
  res
}

#' @rdname add_tailor
#' @export
remove_tailor <- function(x) {
  validate_is_workflow(x)

  if (!has_postprocessor(x)) {
    cli_warn("The workflow has no tailor postprocessor to remove.")
  }

  actions <- x$post$actions
  actions[["tailor"]] <- NULL

  # note that the preprocessor and model fit don't need to be "untrained"
  # with new_stage_* since they are unaffected by the post-processor.
  new_workflow(
    pre = x$pre,
    fit = x$fit,
    post = new_stage_post(actions = actions),
    trained = FALSE
  )
}

#' @rdname add_tailor
#' @export
update_tailor <- function(x, tailor, ...) {
  check_dots_empty()
  x <- remove_tailor(x)
  add_tailor(x, tailor)
}

# ------------------------------------------------------------------------------

#' @export
fit.action_tailor <- function(object, workflow, data, ...) {
  tailor <- object$tailor

  # mock trained workflow to allow for prediction without a post-processor.
  workflow_mock <- mock_trained_workflow(workflow)

  post_fit <-
    fit(
      object = tailor,
      .data = augment(workflow_mock, data),
      outcome = names(extract_mold(workflow_mock)$outcomes),
      estimate = tidyselect::any_of(c(".pred", ".pred_class")),
      probabilities = c(
        tidyselect::contains(".pred_"),
        -tidyselect::matches("^\\.pred$|^\\.pred_class$")
      )
    )

  new_workflow(
    pre = workflow$pre,
    fit = workflow$fit,
    post = new_stage_post(
      actions = workflow$post$actions,
      fit = post_fit
    )
  )
}

# make a version of the workflow that does no post-processing and has its
# `trained` flag set to TRUE
mock_trained_workflow <- function(workflow) {
  workflow <- remove_tailor(workflow)
  workflow <- set_trained(workflow, TRUE)

  workflow
}

# ------------------------------------------------------------------------------

new_action_tailor <- function(tailor, ..., call = caller_env()) {
  check_dots_empty()

  if (!is_tailor(tailor)) {
    cli_abort("{.arg tailor} must be a tailor.", call = call)
  }

  if (tailor::tailor_fully_trained(tailor)) {
    cli_abort("Can't add a trained tailor to a workflow.", call = call)
  }

  new_action_post(
    tailor = tailor,
    subclass = "action_tailor"
  )
}

is_tailor <- function(x) {
  inherits(x, "tailor")
}
