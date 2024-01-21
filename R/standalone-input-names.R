# ---
# repo: tidymodels/workflows
# file: standalone-input-names.R
# last-updated: 2024-01-21
# license: https://unlicense.org
# ---

# secret gist at: https://gist.github.com/topepo/17d51cafcd0ac8dff0552198d6aeadbf

# This file provides a portable set of helper functions for determining the
# names of the predictor columns used as inputs into a workflow.

# ## Changelog
# 2024-01-21
# * First version

# ------------------------------------------------------------------------------

check_workflow_fit <- function(x) {
  if (!x$trained) {
    stop("The workflow should be trainined.")
  }
  invisible(NULL)
}

check_recipe_fit <- function(x) {
  is_trained <- vapply(x$steps, function(x) x$trained, logical(1))
  if (!all(is_trained)) {
    stop("All recipe steps should be trainined.")
  }
  invisible(NULL)
}

blueprint_ptype <- function(x) {
  names(x$pre$mold$blueprint$ptypes$predictors)
}

.get_input_predictors_workflow <- function(x, ...) {
  check_workflow_fit(x)
  # We can get the columns that are inputs to the recipe but some of these may
  # not be predictors. We'll interrogate the recipe and pull out the current
  # predictor names from the original input
  if ("recipe" %in% names(x$pre$actions)) {
    mold <- x$pre$mold
    rec <- mold$blueprint$recipe
    res <- .get_input_predictors_recipe(rec)
  } else {
    res <- blueprint_ptype(x)
  }
  sort(unique(res))
}

is_predictor_role <- function(x) {
  vapply(x$role, function(x) any(x == "predictor"), logical(1))
}

.get_input_predictors_recipe <- function(x, ...) {
  check_recipe_fit(x)
  var_info <- x$last_term_info

  keep_rows <- var_info$source == "original" & is_predictor_role(var_info)
  var_info <- var_info[keep_rows,]
  var_info$variable
}
