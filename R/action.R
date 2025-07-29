add_action <- function(x, action, name, ..., call = caller_env()) {
  validate_is_workflow(x, call = call)

  check_conflicts(action, x, call = call)

  add_action_impl(x, action, name, call = call)
}

# ------------------------------------------------------------------------------

add_action_impl <- function(x, action, name, ..., call = caller_env()) {
  check_dots_empty()
  UseMethod("add_action_impl", action)
}

#' @export
add_action_impl.action_pre <- function(
  x,
  action,
  name,
  ...,
  call = caller_env()
) {
  check_singleton(x$pre$actions, name, call = call)
  x$pre <- add_action_to_stage(x$pre, action, name, order_stage_pre())
  x
}

#' @export
add_action_impl.action_fit <- function(
  x,
  action,
  name,
  ...,
  call = caller_env()
) {
  check_singleton(x$fit$actions, name, call = call)
  x$fit <- add_action_to_stage(x$fit, action, name, order_stage_fit())
  x
}

#' @export
add_action_impl.action_post <- function(
  x,
  action,
  name,
  ...,
  call = caller_env()
) {
  check_singleton(x$post$actions, name, call = call)
  x$post <- add_action_to_stage(x$post, action, name, order_stage_post())
  x
}

# ------------------------------------------------------------------------------

order_stage_pre <- function() {
  # Case weights must come before preprocessor
  c(
    c("case_weights"),
    c("formula", "recipe", "variables")
  )
}

order_stage_fit <- function() {
  "model"
}

order_stage_post <- function() {
  "tailor"
}

# ------------------------------------------------------------------------------

add_action_to_stage <- function(stage, action, name, order) {
  actions <- c(stage$actions, list2(!!name := action))

  # Apply required ordering for this stage
  order <- intersect(order, names(actions))
  actions <- actions[order]

  stage$actions <- actions

  stage
}

# ------------------------------------------------------------------------------

# `check_conflicts()` allows us to to check that no other action interferes
# with the current action. For instance, we can't have a formula action with
# a recipe action

check_conflicts <- function(action, x, ..., call = caller_env()) {
  check_dots_empty()
  UseMethod("check_conflicts")
}

#' @export
check_conflicts.default <- function(action, x, ..., call = caller_env()) {
  invisible(action)
}

# ------------------------------------------------------------------------------

check_singleton <- function(actions, name, ..., call = caller_env()) {
  check_dots_empty()

  if (name %in% names(actions)) {
    cli_abort(
      "A `{name}` action has already been added to this workflow.",
      call = call
    )
  }

  invisible(actions)
}

# ------------------------------------------------------------------------------

new_action_pre <- function(..., subclass = character()) {
  new_action(..., subclass = c(subclass, "action_pre"))
}

new_action_fit <- function(..., subclass = character()) {
  new_action(..., subclass = c(subclass, "action_fit"))
}

new_action_post <- function(..., subclass = character()) {
  new_action(..., subclass = c(subclass, "action_post"))
}

is_action_pre <- function(x) {
  inherits(x, "action_pre")
}

is_action_fit <- function(x) {
  inherits(x, "action_fit")
}

is_action_post <- function(x) {
  inherits(x, "action_post")
}

# ------------------------------------------------------------------------------

# An `action` is a list of objects that define how to perform a specific action,
# such as working with a recipe, or formula terms, or a model

new_action <- function(..., subclass = character()) {
  data <- list2(...)

  if (!is_uniquely_named(data)) {
    cli_abort("All elements of `...` must be uniquely named.", .internal = TRUE)
  }

  structure(data, class = c(subclass, "action"))
}

is_action <- function(x) {
  inherits(x, "action")
}

# ------------------------------------------------------------------------------

is_list_of_actions <- function(x) {
  x <- compact(x)

  all(map_lgl(x, is_action))
}
