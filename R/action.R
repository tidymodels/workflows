add_action <- function(x, action, name) {
  if (!is_workflow(x)) {
    abort("`x` must be a workflow.")
  }

  check_conflicts(action, x)

  if (is_action_pre(action)) {
    x$pre <- add_action_to_stage(x$pre, action, name)
  } else if (is_action_fit(action)) {
    x$fit <- add_action_to_stage(x$fit, action, name)
  } else if (is_action_post(action)) {
    x$post <- add_action_to_stage(x$post, action, name)
  }

  x
}

add_action_to_stage <- function(stage, action, name) {
  stage$actions <- c(stage$actions, list2(!!name := action))
  stage
}

# ------------------------------------------------------------------------------

# `check_conflicts()` allows us to to check that no other action interferes
# with the current action. For instance, we can't have a formula action with
# a recipe action

check_conflicts <- function(action, x) {
  UseMethod("check_conflicts")
}

check_conflicts.default <- function(action, x) {
  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_pre <- function(done = FALSE, ..., subclass = character()) {
  new_action(done = done, ..., subclass = c(subclass, "action_pre"))
}

new_action_fit <- function(done = FALSE, ..., subclass = character()) {
  new_action(done = done, ..., subclass = c(subclass, "action_fit"))
}

new_action_post <- function(done = FALSE, ..., subclass = character()) {
  new_action(done = done, ..., subclass = c(subclass, "action_post"))
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

new_action <- function(done = FALSE, ..., subclass = character()) {
  vec_assert(done, ptype = logical(), size = 1L)

  data <- list2(done = done, ...)

  if (!is_uniquely_named(data)) {
    abort("All elements of `...` must be uniquely named.")
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
