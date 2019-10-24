#' Create a workflow
#'
#' A `workflow` is a container object that aggregates information required to
#' fit and predict from a model. This information might be a recipe used in
#' preprocessing, specified through [add_recipe()], or the model specification
#' to fit, specified through [add_model()].
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(mpg ~ cyl, mtcars)
#' rec <- step_log(rec, cyl)
#'
#' wrk <- workflow()
#' wrk <- add_recipe(wrk, rec)
#'
#' @export
workflow <- function() {
  new_workflow()
}

new_workflow <- function(pre = new_stage_pre(), fit = new_stage_fit(), post = new_stage_post()) {
  if (!is_stage(pre)) {
    abort("`pre` must be a `stage`.")
  }

  if (!is_stage(fit)) {
    abort("`fit` must be a `stage`.")
  }

  if (!is_stage(post)) {
    abort("`post` must be a `stage`.")
  }

  data <- list(
    pre = pre,
    fit = fit,
    post = post
  )

  structure(data, class = "workflow")
}

is_workflow <- function(x) {
  inherits(x, "workflow")
}
