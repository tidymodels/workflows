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

has_mold <- function(x) {
  !is.null(x$pre$mold)
}

has_spec <- function(x) {
  "model" %in% names(x$fit$actions)
}

has_fit <- function(x) {
  !is.null(x$fit$fit)
}
