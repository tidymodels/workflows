# TODO - What else?

#' @export
ctrl_workflow <- function(parsnip = fit_control()) {
  data <- list(
    parsnip = parsnip
  )

  structure(data, class = "ctrl_workflow")
}
