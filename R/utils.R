is_uniquely_named <- function(x) {
  if (length(x) > 0) {
    is_named(x) && !anyDuplicated(names(x))
  } else {
    TRUE
  }
}

glubort <- function (..., .sep = "", .envir = parent.frame()) {
  abort(glue::glue(..., .sep = .sep, .envir = .envir))
}

is_model_fit <- function(x) {
  inherits(x, "model_fit")
}

is_model_spec <- function(x) {
  inherits(x, "model_spec")
}

validate_recipes_available <- function() {
  if (!requireNamespace("recipes", quietly = TRUE)) {
    abort(
      "The `recipes` package must be available to add a recipe."
    )
  }
  invisible()
}

# ------------------------------------------------------------------------------

# https://github.com/r-lib/tidyselect/blob/10e00cea2fff3585fc827b6a7eb5e172acadbb2f/R/utils.R#L109
vec_index_invert <- function (x) {
  if (vec_index_is_empty(x)) {
    TRUE
  }
  else {
    -x
  }
}

vec_index_is_empty <- function (x) {
  !length(x) || all(x == 0L)
}

# ------------------------------------------------------------------------------

validate_is_workflow <- function(x, arg = "`x`") {
  if (!is_workflow(x)) {
    glubort("{arg} must be a workflow, not a {class(x)[[1]]}.")
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

has_mold <- function(x) {
  !is.null(x$pre$mold)
}

has_spec <- function(x) {
  "model" %in% names(x$fit$actions)
}

has_fit <- function(x) {
  !is.null(x$fit$fit)
}

has_blueprint <- function(x) {
  if (has_preprocessor_formula(x)) {
    !is.null(x$pre$actions$formula$blueprint)
  } else if (has_preprocessor_recipe(x)) {
    !is.null(x$pre$actions$recipe$blueprint)
  } else if (has_preprocessor_variables(x)) {
    !is.null(x$pre$actions$variables$blueprint)
  } else {
    abort("Internal error: `x` must have a preprocessor to check for a blueprint.")
  }
}
