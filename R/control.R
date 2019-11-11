# TODO - What else?

#' Control object for a workflow
#'
#' `control_workflow()` holds the control parameters for a workflow.
#'
#' @param control_parsnip A parsnip control object. If `NULL`, a default control
#'   argument is constructed from [parsnip::control_parsnip()].
#'
#' @export
control_workflow <- function(control_parsnip = NULL) {
  if (is.null(control_parsnip)) {
    control_parsnip <- parsnip::control_parsnip()
  }

  data <- list(
    control_parsnip = control_parsnip
  )

  structure(data, class = "control_workflow")
}
