#' @export
fit.workflow <- function(object, data, ..., ctrl = ctrl_workflow()) {
  workflow <- object

  if (is_missing(data)) {
    abort("`data` must be provided to fit a workflow.")
  }

  ellipsis::check_dots_empty()
  validate_has_minimal_components(object)

  result <- .fit_pre(workflow, data)
  workflow <- result$workflow
  data <- result$data

  result <- .fit_model(workflow, data, ctrl)
  workflow <- result$workflow

  # Eh? Predictions during the fit?
  # pred <- result$pred
  # result <- fit_post(workflow, pred)

  workflow
}

# ------------------------------------------------------------------------------

.fit_pre <- function(workflow, data) {
  n <- vec_size(workflow[["pre"]]$actions)

  for(i in seq_len(n)) {
    action <- workflow[["pre"]]$actions[[i]]

    result <- fit(action, workflow = workflow, data = data)
    workflow <- result$workflow
    data <- result$data
  }

  list(workflow = workflow, data = data)
}

# Just one action to do?
.fit_model <- function(workflow, data, ctrl) {
  action_model <- workflow[["fit"]][["actions"]][["model"]]
  fit(action_model, workflow = workflow, data = data, ctrl = ctrl)
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
