# TODO - What else?

#' Control object for a workflow
#'
#' `control_workflow()` holds the control parameters for a workflow.
#'
#' @param control_parsnip A parsnip control object.
#'
#' @export
control_workflow <- function(control_parsnip = fit_control()) {
  data <- list(
    control_parsnip = control_parsnip
  )

  structure(data, class = "control_workflow")
}
