#' @export
.censoring_weights_graf.workflow <- function(
  object,
  predictions,
  cens_predictors = NULL,
  trunc = 0.05,
  eps = 10^-10,
  ...
) {
  if (is.null(object$fit$fit)) {
    cli_abort("The workflow does not have a model fit object.")
  }
  .censoring_weights_graf(
    object$fit$fit,
    predictions,
    cens_predictors,
    trunc,
    eps
  )
}
