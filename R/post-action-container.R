#' Add a container to a workflow
#'
#' @description
#' - `add_container()` specifies post-processing steps to apply through the
#'   usage of a container.
#'
#' - `remove_container()` removes the container as well as any downstream objects
#'   that might get created after the container is used for post-processing, such as
#'   the fitted container.
#'
#' - `update_container()` first removes the container, then replaces the postvious
#'   container with the new one.
#'
#' @param x A workflow
#'
#' @param container A container created using [container::container()]. The container
#'   should not have been trained already with [container::fit()]; workflows
#'   will handle training internally.
#'
#' @param ... Not used.
#'
#' @return
#' `x`, updated with either a new or removed container postprocessor.
#'
#' @export
#' @examples
#' library(container)
#' library(magrittr)
#'
#' container <- container() %>%
#'   adjust_probability_threshold(.1)
#'
#' workflow <- workflow() %>%
#'   add_container(container)
#'
#' workflow
#'
#' remove_container(workflow)
#'
#' update_container(workflow, container() %>% adjust_probability_threshold(.2))
add_container <- function(x, container, ...) {
  check_dots_empty()
  validate_container_available()
  action <- new_action_container(container)
  add_action(x, action, "container")
}

#' @rdname add_container
#' @export
remove_container <- function(x) {
  validate_is_workflow(x)

  if (!has_post(x)) {
    rlang::warn("The workflow has no container postprocessor to remove.")
  }

  actions <- x$post$actions
  actions[["container"]] <- NULL

  # note that the preprocessor and model fit don't need to be "untrained"
  # with new_stage_* since they are unaffected by the post-processor.
  new_workflow(
    pre = x$pre,
    fit = x$fit,
    post = new_stage_post(actions = actions),
    trained = FALSE
  )
}

#' @rdname add_container
#' @export
update_container <- function(x, container, ...) {
  check_dots_empty()
  x <- remove_container(x)
  add_container(x, container)
}

# ------------------------------------------------------------------------------

#' @export
fit.action_container <- function(object, workflow, data, ...) {
  container <- object$container

  # mock trained workflow to allow for prediction without a post-processor.
  workflow_mock <- mock_trained_workflow(workflow)

  post <-
    fit(
      object = container,
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
      post = post
    )
  )
}

# make a version of the workflow that does no post-processing and has its
# `trained` flag set to TRUE
mock_trained_workflow <- function(workflow) {
  workflow <- remove_container(workflow)
  workflow <- set_trained(workflow, TRUE)

  workflow
}

# ------------------------------------------------------------------------------

#' @export
check_conflicts.action_container <- function(action, x, ..., call = caller_env()) {
  post <- x$post

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_container <- function(container, ..., call = caller_env()) {
  check_dots_empty()

  if (!is_container(container)) {
    abort("`container` must be a container.", call = call)
  }

  # todo fully_trained() should be from container
  if (container_fully_trained(container)) {
    abort("Can't add a trained container to a workflow.", call = call)
  }

  new_action_post(
    container = container,
    subclass = "action_container"
  )
}

is_container <- function(x) {
  inherits(x, "container")
}

container_fully_trained <- function(x) {
  if (length(x$operations) == 0L) {
    return(FALSE)
  }

  all(map_lgl(x$operations, container_operation_trained))
}

container_operation_trained <- function(x) {
  isTRUE(x$trained)
}
