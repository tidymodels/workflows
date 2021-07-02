#' Extract elements of a workflow
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' Please use the `extract_*()` functions instead of these (e.g. [extract_mold()]).
#'
#' These functions extract various elements from a workflow object. If they do
#' not exist yet, an error is thrown.
#'
#' - `pull_workflow_preprocessor()` returns the formula, recipe, or variable
#'   expressions used for preprocessing.
#'
#' - `pull_workflow_spec()` returns the parsnip model specification.
#'
#' - `pull_workflow_fit()` returns the parsnip model fit.
#'
#' - `pull_workflow_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe or the formula terms object.
#'
#' - `pull_workflow_prepped_recipe()` returns the prepped recipe. It is
#'   extracted from the mold object returned from `pull_workflow_mold()`.
#'
#' @param x A workflow
#'
#' @return
#' The extracted value from the workflow, `x`, as described in the description
#' section.
#'
#' @name workflow-extractors
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' model <- linear_reg() %>%
#'   set_engine("lm")
#'
#' recipe <- recipe(mpg ~ cyl + disp, mtcars) %>%
#'   step_log(disp)
#'
#' base_wf <- workflow() %>%
#'   add_model(model)
#'
#' recipe_wf <- add_recipe(base_wf, recipe)
#' formula_wf <- add_formula(base_wf, mpg ~ cyl + log(disp))
#' variable_wf <- add_variables(base_wf, mpg, c(cyl, disp))
#'
#' fit_recipe_wf <- fit(recipe_wf, mtcars)
#' fit_formula_wf <- fit(formula_wf, mtcars)
#'
#' # The preprocessor is a recipes, formula, or a list holding the
#' # tidyselect expressions identifying the outcomes/predictors
#' pull_workflow_preprocessor(recipe_wf)
#' pull_workflow_preprocessor(formula_wf)
#' pull_workflow_preprocessor(variable_wf)
#'
#' # The `spec` is the parsnip spec before it has been fit.
#' # The `fit` is the fit parsnip model.
#' pull_workflow_spec(fit_formula_wf)
#' pull_workflow_fit(fit_formula_wf)
#'
#' # The mold is returned from `hardhat::mold()`, and contains the
#' # predictors, outcomes, and information about the preprocessing
#' # for use on new data at `predict()` time.
#' pull_workflow_mold(fit_recipe_wf)
#'
#' # A useful shortcut is to extract the prepped recipe from the workflow
#' pull_workflow_prepped_recipe(fit_recipe_wf)
#'
#' # That is identical to
#' identical(
#'   pull_workflow_mold(fit_recipe_wf)$blueprint$recipe,
#'   pull_workflow_prepped_recipe(fit_recipe_wf)
#' )
NULL

#' @rdname workflow-extractors
#' @export
pull_workflow_preprocessor <- function(x) {
  lifecycle::deprecate_soft("0.2.3", "pull_workflow_preprocessor()", "extract_preprocessor()")

  validate_is_workflow(x)

  if (has_preprocessor_formula(x)) {
    return(x$pre$actions$formula$formula)
  }

  if (has_preprocessor_recipe(x)) {
    return(x$pre$actions$recipe$recipe)
  }

  if (has_preprocessor_variables(x)) {
    return(x$pre$actions$variables$variables)
  }

  abort("The workflow does not have a preprocessor.")
}

#' @rdname workflow-extractors
#' @export
pull_workflow_spec <- function(x) {
  lifecycle::deprecate_soft("0.2.3", "pull_workflow_spec()", "extract_spec_parsnip()")

  validate_is_workflow(x)

  if (has_spec(x)) {
    return(x$fit$actions$model$spec)
  }

  abort("The workflow does not have a model spec.")
}

#' @rdname workflow-extractors
#' @export
pull_workflow_fit <- function(x) {
  lifecycle::deprecate_soft("0.2.3", "pull_workflow_fit()", "extract_fit_parsnip()")

  validate_is_workflow(x)

  if (has_fit(x)) {
    return(x$fit$fit)
  }

  abort("The workflow does not have a model fit. Have you called `fit()` yet?")
}

#' @rdname workflow-extractors
#' @export
pull_workflow_mold <- function(x) {
  lifecycle::deprecate_soft("0.2.3", "pull_workflow_mold()", "extract_mold()")

  validate_is_workflow(x)

  if (has_mold(x)) {
    return(x$pre$mold)
  }

  abort("The workflow does not have a mold. Have you called `fit()` yet?")
}

#' @rdname workflow-extractors
#' @export
pull_workflow_prepped_recipe <- function(x) {
  lifecycle::deprecate_soft("0.2.3", "pull_workflow_prepped_recipe()", "extract_recipe()")

  validate_is_workflow(x)

  if (!has_preprocessor_recipe(x)) {
    abort("The workflow must have a recipe preprocessor.")
  }

  mold <- pull_workflow_mold(x)

  mold$blueprint$recipe
}
