is_uniquely_named <- function(x) {
  if (vec_size(x) > 0) {
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

check_existing_object <- function(x, type = c("formula", "recipe", "model")) {
  type <- match.arg(type)

  loc <-  switch(type,
                 formula = , recipe = x$pre$actions,
                 model = x$fit$actions)

  if (!any(names(loc) == type)) {
    rlang::warn(glue::glue("The workflow has no {type} to remove."))
  }
  invisible(NULL)
}


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
