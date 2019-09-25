# A director is the highest level of object type

new_fit_director <- function(workflow, data, ctrl) {
  data <- list(
    workflow = workflow,
    data = data,
    ctrl = ctrl
  )

  structure(data, class = "fit_director")
}
