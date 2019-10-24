# TODO - What else?

#' Control object for a workflow
#'
#' `ctrl_workflow()` holds the control parameters for a workflow.
#'
#' @param parsnip A parsnip control object.
#'
#' @export
ctrl_workflow <- function(parsnip = fit_control()) {
  data <- list(
    parsnip = parsnip
  )

  structure(data, class = "ctrl_workflow")
}
