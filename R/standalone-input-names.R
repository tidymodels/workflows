# ---
# repo: tidymodels/workflows
# file: standalone-input-names.R
# last-updated: 2024-01-291
# license: https://unlicense.org
# requires: cli, rlang
# ---

# This file provides a portable set of helper functions for determining the
# names of the predictor columns used as inputs into a workflow.

# ## Changelog
# 2024-01-21
# * First version
# 2024-01-29
# * Changes after PR review

# nocov start

# ------------------------------------------------------------------------------
# Primary functions

# @param x A _fitted_ workflow or recipe.
# @param call An environment indicating where the top-level function was invoked
# to print out better errors.
# @return A character vector of sorted columns names.

.get_input_predictors_workflow <- function(x, ..., call = rlang::current_env()) {
  check_workflow_fit(x, call = call)
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

.get_input_predictors_recipe <- function(x, ..., call = rlang::current_env()) {
  check_recipe_fit(x, call = call)
  var_info <- x$last_term_info

  keep_rows <- var_info$source == "original" & is_predictor_role(var_info)
  var_info <- var_info[keep_rows,]
  var_info$variable
}

.get_input_outcome_workflow <- function(x) {
  check_workflow_fit(x)
  names(x$pre$mold$blueprint$ptypes$outcomes)
}

# ------------------------------------------------------------------------------
# Helper functions

check_workflow_fit <- function(x, call) {
  if (!x$trained) {
    cli::cli_abort("The workflow should be trained.", call = call)
  }
  invisible(NULL)
}

check_recipe_fit <- function(x, call) {
  is_trained <- vapply(x$steps, function(x) x$trained, logical(1))
  if (!all(is_trained)) {
    cli::cli_abort("All recipe steps should be trained.", call = call)
  }
  invisible(NULL)
}

blueprint_ptype <- function(x) {
  names(x$pre$mold$blueprint$ptypes$predictors)
}

is_predictor_role <- function(x) {
  vapply(x$role, function(x) any(x == "predictor"), logical(1))
}

# nocov end
