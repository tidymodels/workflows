#' Tidy a workflow
#'
#' @description
#' This is a [generics::tidy()] method for a workflow that calls `tidy()` on
#' either the underlying parsnip model or the recipe, depending on the value
#' of `what`.
#'
#' `x` must be a fitted workflow, resulting in fitted parsnip model or prepped
#' recipe that you want to tidy.
#'
#' @details
#' To tidy the unprepped recipe, use [pull_workflow_preprocessor()] and `tidy()`
#' that directly.
#'
#' @inheritParams generics::tidy
#'
#' @param what A single string. Either `"model"` or `"recipe"` to select
#'   which part of the workflow to tidy. Defaults to tidying the model.
#'
#' @export
tidy.workflow <- function(x, what = "model", ...) {
  what <- arg_match(what, values = c("model", "recipe"))

  if (identical(what, "model")) {
    x <- pull_workflow_fit(x)
    out <- tidy(x, ...)
    return(out)
  }

  if (identical(what, "recipe")) {
    x <- pull_workflow_prepped_recipe(x)
    out <- tidy(x, ...)
    return(out)
  }

  abort("Internal error: `what` must be 'model' or 'recipe'.")
}
