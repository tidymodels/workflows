#' @export
predict.workflow <- function(object, new_data, type = NULL, opts = list(), ...) {
  workflow <- object

  if (!workflow$run) {
    abort("Workflow has not yet been fit. Do you need to call `fit()`?")
  }

  blueprint <- workflow$pre$mold$blueprint
  forged <- hardhat::forge(new_data, blueprint)
  new_data <- forged$predictors

  fit <- workflow$fit$fit

  predict(fit, new_data, type = type, opts = opts, ...)
}
