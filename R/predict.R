#' @export
predict.workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  if (!object$run) {
    abort("Workflow has not yet been fit. Do you need to call `fit()`?")
  }

  blueprint <- object$pre$mold$blueprint
  forged <- hardhat::forge(new_data, blueprint)
  new_data <- forged$predictors

  fit <- object$fit$fit

  predict(fit, new_data, type = type, opts = opts, ...)
}
