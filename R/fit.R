#' @export
fit.workflow <- function(object, data, ..., ctrl = ctrl_workflow()) {
  if (is_missing(data)) {
    abort("`data` must be provided to fit a workflow.")
  }

  ellipsis::check_dots_empty()
  validate_has_minimal_components(object)

  director <- new_fit_director(object, data, ctrl)

  director <- fit_stage(director, "pre")
  director <- fit_stage(director, "fit")
  director <- fit_stage(director, "post")

  director$workflow
}

# ------------------------------------------------------------------------------

fit_stage <- function(director, stage) {
  n <- vec_size(director$workflow[[stage]]$actions)

  for(i in seq_len(n)) {
    action <- director$workflow[[stage]]$actions[[i]]
    director <- fit(action, director)
  }

  director
}

# ------------------------------------------------------------------------------

validate_has_minimal_components <- function(x) {
  has_preprocessor <- has_action(x$pre, "formula") || has_action(x$pre, "recipe")

  if (!has_preprocessor) {
    glubort(
      "The workflow must have a formula or recipe preprocessor. ",
      "Provide one with `add_formula()` or `add_recipe()`."
    )
  }

  has_model <- has_action(x$fit, "model")

  if (!has_model) {
    glubort(
      "The workflow must have a model. ",
      "Provide one with `add_model()`."
    )
  }

  invisible(x)
}
