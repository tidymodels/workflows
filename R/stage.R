new_stage_pre <- function(actions = list(), mold = NULL) {
  if (!is.null(mold) && !is.list(mold)) {
    abort("`mold` must be a result of calling `hardhat::mold()`.")
  }

  new_stage(actions = actions, mold = mold, subclass = "stage_pre")
}

new_stage_fit <- function(actions = list(), fit = NULL) {
  if (!is.null(fit) && !is_model_fit(fit)) {
    abort("`fit` must be a `model_fit`.")
  }

  new_stage(actions = actions, fit = fit, subclass = "stage_fit")
}

new_stage_post <- function(actions = list()) {
  new_stage(actions, subclass = "stage_post")
}

# ------------------------------------------------------------------------------

# A `stage` is a collection of `action`s

# There are 3 stages that actions can fall into:
# - pre
# - fit
# - post

new_stage <- function(actions = list(), ..., subclass = character()) {
  if (!is_list_of_actions(actions)) {
    abort("`actions` must be a list of actions.")
  }

  if (!is_uniquely_named(actions)) {
    abort("`actions` must be uniquely named.")
  }

  fields <- list2(...)

  if (!is_uniquely_named(fields)) {
    abort("`...` must be uniquely named.")
  }

  fields <- list2(actions = actions, !!! fields)

  structure(fields, class = c(subclass, "stage"))
}

# ------------------------------------------------------------------------------

is_stage <- function(x) {
  inherits(x, "stage")
}

has_action <- function(stage, name) {
  name %in% names(stage$actions)
}

# ------------------------------------------------------------------------------
