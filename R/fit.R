#' @export
fit.workflow <- function(object, data, ..., ctrl = ctrl_workflow()) {
  workflow <- object

  if (is_missing(data)) {
    abort("`data` must be provided to fit a workflow.")
  }

  ellipsis::check_dots_empty()
  validate_has_minimal_components(object)

  workflow <- .fit_pre(workflow, data)
  workflow <- .fit_model(workflow, ctrl)

  # Eh? Predictions during the fit?
  # pred <- result$pred
  # result <- fit_post(workflow, pred)

  workflow
}

# ------------------------------------------------------------------------------

#' @export
.fit_pre <- function(workflow, data) {
  n <- vec_size(workflow[["pre"]]$actions)

  for(i in seq_len(n)) {
    action <- workflow[["pre"]]$actions[[i]]

    # Update both the `workflow` and the `data` as we iterate through pre steps
    result <- fit(action, workflow = workflow, data = data)
    workflow <- result$workflow
    data <- result$data
  }

  # But only return the workflow, it contains the final set of data in `mold`
  workflow
}

#' @export
.fit_model <- function(workflow, ctrl) {
  action_model <- workflow[["fit"]][["actions"]][["model"]]
  fit(action_model, workflow = workflow, ctrl = ctrl)
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
