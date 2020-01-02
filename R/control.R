#' Control object for a workflow
#'
#' `control_workflow()` holds the control parameters for a workflow.
#'
#' @param control_parsnip A parsnip control object. If `NULL`, a default control
#'   argument is constructed from [parsnip::control_parsnip()].
#'
#' @return
#' A `control_workflow` object for tweaking the workflow fitting process.
#'
#' @export
#' @examples
#' control_workflow()
control_workflow <- function(control_parsnip = NULL) {
  control_parsnip <- check_control_parsnip(control_parsnip)

  data <- list(
    control_parsnip = control_parsnip
  )

  structure(data, class = "control_workflow")
}

#' @export
print.control_workflow <- function(x, ...) {
  cat("<control_workflow>")
  invisible()
}

check_control_parsnip <- function(x) {
  if (is.null(x)) {
    x <- parsnip::control_parsnip()
  }

  if (!inherits(x, "control_parsnip")) {
    abort("`control_parsnip` must be a 'control_parsnip' object.")
  }

  x
}
