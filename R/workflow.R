#' @export
workflow <- function() {
  new_workflow()
}

new_workflow <- function(pre = list(), fit = list(), post = list()) {
  if (!is.list(pre)) {
    abort("`pre` must be a list.")
  }

  if (!is.list(fit)) {
    abort("`fit` must be a list.")
  }

  if (!is.list(post)) {
    abort("`post` must be a list.")
  }

  data <- list(
    pre = pre,
    fit = fit,
    post = post
  )

  structure(data, class = "workflow")
}
