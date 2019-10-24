# TODO - What else?

#' Control object for a workflow
#'
#' `ctrl_workflow()` holds the control parameters for a workflow.
#'
#' @param ctrl_parsnip A parsnip control object.
#'
#' @export
ctrl_workflow <- function(ctrl_parsnip = fit_control()) {
  data <- list(
    ctrl_parsnip = ctrl_parsnip
  )

  structure(data, class = "ctrl_workflow")
}
