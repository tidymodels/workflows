new_stage_pre <- function(
  actions = new_named_list(),
  mold = NULL,
  case_weights = NULL
) {
  if (!is.null(mold) && !is.list(mold)) {
    cli_abort(
      "{.arg mold} must be a result of calling {.fun hardhat::mold}.",
      .internal = TRUE
    )
  }

  if (!is_null(case_weights) && !hardhat::is_case_weights(case_weights)) {
    cli_abort(
      "{.arg case_weights} must be a true case weights column.",
      .internal = TRUE
    )
  }

  new_stage(
    actions = actions,
    mold = mold,
    case_weights = case_weights,
    subclass = "stage_pre"
  )
}

new_stage_fit <- function(actions = new_named_list(), fit = NULL) {
  if (!is.null(fit) && !is_model_fit(fit)) {
    cli_abort("{.arg fit} must be a {.cls model_fit}.", .internal = TRUE)
  }

  new_stage(actions = actions, fit = fit, subclass = "stage_fit")
}

new_stage_post <- function(actions = new_named_list(), fit = NULL) {
  if (!is.null(fit) && !is_tailor(fit)) {
    cli_abort("{.arg fit} must be a fitted {.cls tailor}.", .internal = TRUE)
  }

  new_stage(actions, fit = fit, subclass = "stage_post")
}

# ------------------------------------------------------------------------------

# A `stage` is a collection of `action`s

# There are 3 stages that actions can fall into:
# - pre
# - fit
# - post

new_stage <- function(actions = new_named_list(), ..., subclass = character()) {
  if (!is_list_of_actions(actions)) {
    cli_abort("{.arg actions} must be a list of actions.", .internal = TRUE)
  }

  if (!is_uniquely_named(actions)) {
    cli_abort("{.arg actions} must be uniquely named.", .internal = TRUE)
  }

  fields <- list2(...)

  if (!is_uniquely_named(fields)) {
    cli_abort("`...` must be uniquely named.", .internal = TRUE)
  }

  fields <- list2(actions = actions, !!!fields)

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

new_named_list <- function() {
  # To standardize results for testing.
  # Mainly applicable when `[[<-` removes all elements from a named list and
  # leaves a named list behind that we want to compare against.
  set_names(list(), character())
}
