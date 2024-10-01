is_uniquely_named <- function(x) {
  if (length(x) > 0) {
    is_named(x) && !anyDuplicated(names(x))
  } else {
    TRUE
  }
}

is_model_fit <- function(x) {
  inherits(x, "model_fit") || modelenv::is_unsupervised_fit(x)
}

is_model_spec <- function(x) {
  inherits(x, "model_spec") || modelenv::is_unsupervised_spec(x)
}

validate_recipes_available <- function(..., call = caller_env()) {
  check_dots_empty()

  if (!requireNamespace("recipes", quietly = TRUE)) {
    cli_abort(
      "The {.pkg recipes} package must be available to add a recipe.",
      call = call
    )
  }

  invisible()
}

validate_tailor_available <- function(..., call = caller_env()) {
  check_dots_empty()

  if (!requireNamespace("tailor", quietly = TRUE)) {
    cli_abort(
      "The {.pkg tailor} package must be available to add a tailor.",
      call = call
    )
  }

  invisible()
}

# ------------------------------------------------------------------------------

# https://github.com/r-lib/tidyselect/blob/10e00cea2fff3585fc827b6a7eb5e172acadbb2f/R/utils.R#L109
vec_index_invert <- function(x) {
  if (vec_index_is_empty(x)) {
    TRUE
  } else {
    -x
  }
}

vec_index_is_empty <- function(x) {
  !length(x) || all(x == 0L)
}

# ------------------------------------------------------------------------------

validate_is_workflow <- function(x, ..., arg = "`x`", call = caller_env()) {
  check_dots_empty()

  if (!is_workflow(x)) {
    cli_abort(
      "{arg} must be a workflow, not a {.cls {class(x)[[1]]}}.",
      call = call
    )
  }

  invisible(x)
}

# ------------------------------------------------------------------------------

has_preprocessor_recipe <- function(x) {
  "recipe" %in% names(x$pre$actions)
}

has_preprocessor_formula <- function(x) {
  "formula" %in% names(x$pre$actions)
}

has_preprocessor_variables <- function(x) {
  "variables" %in% names(x$pre$actions)
}

has_case_weights <- function(x) {
  "case_weights" %in% names(x$pre$actions)
}

has_mold <- function(x) {
  !is.null(x$pre$mold)
}

has_spec <- function(x) {
  "model" %in% names(x$fit$actions)
}

has_fit <- function(x) {
  !is.null(x$fit$fit)
}

has_postprocessor <- function(x) {
  has_postprocessor_tailor(x)
}

has_postprocessor_tailor <- function(x) {
  "tailor" %in% names(x$post$actions)
}

has_blueprint <- function(x) {
  if (has_preprocessor_formula(x)) {
    !is.null(x$pre$actions$formula$blueprint)
  } else if (has_preprocessor_recipe(x)) {
    !is.null(x$pre$actions$recipe$blueprint)
  } else if (has_preprocessor_variables(x)) {
    !is.null(x$pre$actions$variables$blueprint)
  } else {
    cli_abort(
      "{.arg x} must have a preprocessor to check for a blueprint.",
      .internal = TRUE
    )
  }
}
