#' Butcher methods for a workflow
#'
#' These methods allow you to use the butcher package to reduce the size of
#' a workflow. After calling `butcher::butcher()` on a workflow, the only
#' guarantee is that you will still be able to `predict()` from that workflow.
#' Other functions may not work as expected.
#'
#' @param x A workflow.
#' @param verbose Should information be printed about how much memory is freed
#'   from butchering?
#' @param ... Extra arguments possibly used by underlying methods.
#'
#' @name workflow-butcher

# @export - onLoad
#' @rdname workflow-butcher
axe_call.workflow <- function(x, verbose = FALSE, ...) {
  fit <- extract_fit_parsnip(x)
  fit <- butcher::axe_call(fit, verbose = verbose, ...)
  x <- replace_workflow_fit(x, fit)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname workflow-butcher
axe_ctrl.workflow <- function(x, verbose = FALSE, ...) {
  fit <- extract_fit_parsnip(x)
  fit <- butcher::axe_ctrl(fit, verbose = verbose, ...)
  x <- replace_workflow_fit(x, fit)
  add_butcher_class(x)
}

# @export - onLoad
#' @rdname workflow-butcher
axe_data.workflow <- function(x, verbose = FALSE, ...) {
  fit <- extract_fit_parsnip(x)
  fit <- butcher::axe_data(fit, verbose = verbose, ...)
  x <- replace_workflow_fit(x, fit)

  x <- replace_workflow_outcomes(x, NULL)
  x <- replace_workflow_predictors(x, NULL)

  add_butcher_class(x)
}

# @export - onLoad
#' @rdname workflow-butcher
axe_env.workflow <- function(x, verbose = FALSE, ...) {
  fit <- extract_fit_parsnip(x)
  fit <- butcher::axe_env(fit, verbose = verbose, ...)
  x <- replace_workflow_fit(x, fit)

  # Axe env of preprocessor
  preprocessor <- extract_preprocessor(x)

  if (has_preprocessor_recipe(x)) {
    preprocessor <- butcher::axe_env(preprocessor, verbose = verbose, ...)
  } else if (has_preprocessor_formula(x)) {
    preprocessor <- butcher::axe_env(preprocessor, verbose = verbose, ...)
  } else if (has_preprocessor_variables(x)) {
    preprocessor$outcomes <- butcher::axe_env(
      preprocessor$outcomes,
      verbose = verbose,
      ...
    )
    preprocessor$predictors <- butcher::axe_env(
      preprocessor$predictors,
      verbose = verbose,
      ...
    )
  }

  x <- replace_workflow_preprocessor(x, preprocessor)

  # Axe env of prepped recipe (separate from fresh recipe preprocessor)
  if (has_preprocessor_recipe(x)) {
    prepped <- extract_recipe(x)
    prepped <- butcher::axe_env(prepped, verbose = verbose, ...)
    x <- replace_workflow_prepped_recipe(x, prepped)
  }

  add_butcher_class(x)
}

# @export - onLoad
#' @rdname workflow-butcher
axe_fitted.workflow <- function(x, verbose = FALSE, ...) {
  fit <- extract_fit_parsnip(x)
  fit <- butcher::axe_fitted(fit, verbose = verbose, ...)
  x <- replace_workflow_fit(x, fit)

  if (has_preprocessor_recipe(x)) {
    # hardhat already removes the `$template` from the fitted recipe that we get
    # back from `extract_recipe()`, so we only axe the preprocessor recipe here.
    preprocessor <- extract_preprocessor(x)
    preprocessor <- butcher::axe_fitted(preprocessor, verbose = verbose, ...)
    x <- replace_workflow_preprocessor(x, preprocessor)
  }

  add_butcher_class(x)
}

# ------------------------------------------------------------------------------

# butcher:::add_butcher_class
add_butcher_class <- function(x) {
  if (!any(grepl("butcher", class(x)))) {
    class(x) <- append(paste0("butchered_", class(x)[1]), class(x))
  }
  x
}

# ------------------------------------------------------------------------------

# For internal usage only, no checks on `value`. `value` can even be `NULL` to
# remove the element from the list. This is useful for removing
# predictors/outcomes when butchering. This does a direct replacement, with
# no resetting of `trained` or any stages.

replace_workflow_preprocessor <- function(x, value, ..., call = caller_env()) {
  check_dots_empty()

  validate_is_workflow(x, call = call)

  if (has_preprocessor_formula(x)) {
    x$pre$actions$formula$formula <- value
  } else if (has_preprocessor_recipe(x)) {
    x$pre$actions$recipe$recipe <- value
  } else if (has_preprocessor_variables(x)) {
    x$pre$actions$variables$variables <- value
  } else {
    cli_abort("The workflow does not have a preprocessor.", call = call)
  }

  x
}

replace_workflow_fit <- function(x, value, ..., call = caller_env()) {
  check_dots_empty()

  validate_is_workflow(x, call = call)

  if (!has_fit(x)) {
    message <- c(
      "The workflow does not have a model fit.",
      "i" = "Do you need to call {.fun fit}?"
    )
    cli_abort(message, call = call)
  }

  x$fit$fit <- value

  x
}

replace_workflow_predictors <- function(x, value, ..., call = caller_env()) {
  check_dots_empty()

  validate_is_workflow(x, call = call)

  mold <- extract_mold(x)
  mold$predictors <- value

  replace_workflow_mold(x, mold, call = call)
}

replace_workflow_outcomes <- function(x, value, ..., call = caller_env()) {
  check_dots_empty()

  validate_is_workflow(x, call = call)

  mold <- extract_mold(x)
  mold$outcomes <- value

  replace_workflow_mold(x, mold, call = call)
}

replace_workflow_prepped_recipe <- function(
  x,
  value,
  ...,
  call = caller_env()
) {
  check_dots_empty()

  validate_is_workflow(x, call = call)

  if (!has_preprocessor_recipe(x)) {
    cli_abort("The workflow must have a recipe preprocessor.", call = call)
  }

  mold <- extract_mold(x)
  mold$blueprint$recipe <- value

  replace_workflow_mold(x, mold, call = call)
}

replace_workflow_mold <- function(x, value, ..., call = caller_env()) {
  check_dots_empty()

  validate_is_workflow(x, call = call)

  if (!has_mold(x)) {
    cli_abort(
      c(
        "The workflow does not have a mold.",
        "i" = "Have you called {.fun fit} yet?"
      ),
      call = call
    )
  }

  x$pre$mold <- value

  x
}
