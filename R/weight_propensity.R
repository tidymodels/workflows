#' Helper for bridging two-stage causal fits
#'
#' @inherit parsnip::weight_propensity.model_fit description
#'
#' @inheritParams parsnip::weight_propensity.model_fit
#'
#' @inherit parsnip::weight_propensity.model_fit return
#'
#' @inherit parsnip::weight_propensity.model_fit references
#'
#' @importFrom parsnip weight_propensity
#' @method weight_propensity workflow
#' @export
weight_propensity.workflow <- function(object,
                                       wt_fn,
                                       .treated = extract_fit_parsnip(object)$lvl[2],
                                       ...,
                                       data) {
  if (rlang::is_missing(wt_fn) || !is.function(wt_fn)) {
    abort("`wt_fn` must be a function.")
  }

  if (rlang::is_missing(data) || !is.data.frame(data)) {
    abort("`data` must be the data supplied as the data argument to `fit()`.")
  }

  if (!is_trained_workflow(object)) {
    abort("`weight_propensity()` is not well-defined for an unfitted workflow.")
  }

  outcome_name <- names(object$pre$mold$outcomes)

  preds <- predict(object, data, type = "prob")
  preds <- preds[[paste0(".pred_", .treated)]]

  data$.wts <-
    hardhat::importance_weights(
      wt_fn(preds, data[[outcome_name]], .treated = .treated, ...)
    )

  data
}

