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
add_tailor <- function(x, tailor, ...) {
  check_dots_empty()
  validate_tailor_available()
  action <- new_action_tailor(tailor)
  res <- add_action(x, action, "tailor")
  if (should_inner_split(res)) {
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

new_action_tailor <- function(tailor, ..., call = caller_env()) {
  check_dots_empty()

  if (!is_tailor(tailor)) {
    abort("`tailor` must be a tailor.", call = call)
  }

  # todo fully_trained() should be from tailor
  if (tailor_fully_trained(tailor)) {
    abort("Can't add a trained tailor to a workflow.", call = call)
  }

  new_action_post(
    tailor = tailor,
    subclass = "action_tailor"
  )
}

is_tailor <- function(x) {
  inherits(x, "tailor")
}

tailor_fully_trained <- function(x) {
  if (length(x$operations) == 0L) {
    return(FALSE)
  }

  all(map_lgl(x$operations, tailor_operation_trained))
}

tailor_operation_trained <- function(x) {
  isTRUE(x$trained)
}
