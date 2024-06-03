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
#' - `update_tailor()` first removes the tailor, then replaces the postvious
#'   tailor with the new one.
#'
#' @param x A workflow
#'
#' @param tailor A tailor created using [tailor::tailor()]. The tailor
#'   should not have been trained already with [tailor::fit()]; workflows
#'   will handle training internally.
#'
#' @param prop The proportion of the data in [fit.workflow()] that should be
#' held back specifically for estimating the postprocessor. Only relevant for
#' postprocessors that require estimation---see section Data Usage below to
#' learn more. Defaults to 2/3.
#'
#' @param method The method with which to split the data in [fit.workflow()],
#' as a character vector. Only relevant for postprocessors that
#' require estimation and not required when resampling the workflow with
#' tune. If `fit.workflow(data)` arose as `training(split_object)`, this argument can
#' usually be supplied as `class(split_object)`. Defaults to `"mc_split"`, which
#' randomly samples `fit.workflow(data)` into two sets, similarly to
#' [rsample::initial_split()]. See section Data Usage below to learn more.
#'
#' @section Data Usage:
#'
#' While preprocessors and models are trained on data in the usual sense,
#' postprocessors are training on _predictions_ on data. When a workflow
#' is fitted, the user supplies training data with the `data` argument.
#' When workflows don't contain a postprocessor that requires training,
#' they can use all of the supplied `data` to train the preprocessor and model.
#' However, in the case where a postprocessor must be trained as well,
#' training the preprocessor and model on all of `data` would leave no data
#' left to train the postprocessor with---if that were the case, workflows
#' would need to `predict()` from the preprocessor and model on the same `data`
#' that they were trained on, with the postprocessor then training on those
#' predictions. Predictions on data that a model was trained on likely follow
#' different distributions than predictions on unseen data; thus, workflows must
#' split up the supplied `data` into two training sets, where the first is used to
#' train the preprocessor and model and the second is passed to that trained
#' processor and model to generate predictions, which then form the training data
#' for the post-processor.
#'
#' The arguments `prop` and `method` parameterize how that data is split up.
#' `prop` determines the proportion of rows in `fit.workflow(data)` that are
#' allotted to training the preprocessor and model, while the rest are used to
#' train the postprocessor. `method` determines how that split occurs; since
#' `fit.workflow()` just takes in a data frame, the function doesn't have
#' any information on how that dataset came to be. For example, `data` could
#' have been created as:
#'
#' ```
#' split <- rsample::initial_split(some_other_data)
#' data <- rsample::training(split)
#' ```
#'
#' ...in which case it's okay to randomly allot some rows of `data` to train the
#' preprocessor and model and the rest to train the postprocessor. However,
#' `data` could also have arisen as:
#'
#' ```
#' boots <- rsample::bootstraps(some_other_data)
#' split <- rsample::get_rsplit(boots, 1)
#' data <- rsample::assessment(split)
#' ```
#'
#' In this case, some of the rows in `data` will be duplicated. Thus, randomly
#' allotting some of them to train the preprocessor and model and others to train
#' the preprocessor would likely result in the same rows appearing in both
#' datasets, resulting in the preprocessor and model generating predictions on
#' rows they've seen before. Similarly problematic situations could arise in the
#' context of other resampling situations, like time-based splits.
#' The `method` argument ensures that data is allotted properly (and is
#' internally handled by the tune package when resampling workflows).
#'
#' @param ... Not used.
#'
#' @return
#' `x`, updated with either a new or removed tailor postprocessor.
#'
#' @export
#' @examples
#' library(tailor)
#' library(magrittr)
#'
#' tailor <- tailor("binary")
#' tailor_1 <- adjust_probability_threshold(tailor, .1)
#'
#' workflow <- workflow() %>%
#'   add_tailor(tailor_1)
#'
#' workflow
#'
#' remove_tailor(workflow)
#'
#' update_tailor(workflow, adjust_probability_threshold(tailor, .2))
add_tailor <- function(x, tailor, prop = NULL, method = NULL, ...) {
  check_dots_empty()
  validate_tailor_available()
  action <- new_action_tailor(tailor, prop = prop, method = method)
  res <- add_action(x, action, "tailor")
  if (.should_inner_split(res)) {
    validate_rsample_available()
  }
  res
}

#' @rdname add_tailor
#' @export
remove_tailor <- function(x) {
  validate_is_workflow(x)

  if (!has_postprocessor(x)) {
    rlang::warn("The workflow has no tailor postprocessor to remove.")
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

#' @export
check_conflicts.action_tailor <- function(action, x, ..., call = caller_env()) {
  post <- x$post

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_tailor <- function(tailor, prop, method, ..., call = caller_env()) {
  check_dots_empty()

  if (!is_tailor(tailor)) {
    abort("`tailor` must be a tailor.", call = call)
  }

  if (tailor::tailor_fully_trained(tailor)) {
    abort("Can't add a trained tailor to a workflow.", call = call)
  }

  if (!is.null(prop) &&
      (!rlang::is_double(prop, n = 1) || prop <= 0 || prop >= 1)) {
    abort("`prop` must be a numeric on (0, 1).", call = call)
  }

  # todo: test method

  new_action_post(
    tailor = tailor,
    prop = prop,
    method = method,
    subclass = "action_tailor"
  )
}

is_tailor <- function(x) {
  inherits(x, "tailor")
}
