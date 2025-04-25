is_sparse_matrix <- function(x) {
  methods::is(x, "sparseMatrix")
}

# This function takes a workflow and its data. If the model supports sparse data
# And there is a recipe, then it uses `should_use_sparsity()` to determine
# whether all the `sparse = "auto"` should be turned to `"yes"` or `"no"` in the
# recipe.
#
# Done using flow chart in https://github.com/tidymodels/workflows/issues/271
toggle_sparsity <- function(object, data) {
  if (
    allow_sparse(object$fit$actions$model$spec) &&
      has_preprocessor_recipe(object)
  ) {
    est_sparsity <- recipes::.recipes_estimate_sparsity(
      extract_preprocessor(object)
    )

    toggle_sparse <- should_use_sparsity(
      est_sparsity,
      extract_spec_parsnip(object)$engine,
      nrow(data)
    )

    object$pre$actions$recipe$recipe <- recipes::.recipes_toggle_sparse_args(
      object$pre$actions$recipe$recipe,
      choice = toggle_sparse
    )
  }

  object
}

allow_sparse <- function(x) {
  if (inherits(x, "model_fit")) {
    x <- x$spec
  }
  res <- parsnip::get_from_env(paste0(class(x)[1], "_encoding"))
  all(res$allow_sparse_x[res$engine == x$engine])
}

# This function was created using from the output of a mars model fit on the
# simulation data generated in `analysis/time_analysis.R`
# https://github.com/tidymodels/benchmark-sparsity-threshold
#
# The model was extracted using {tidypredict} and hand-tuned for speed.
#
# The model was fit on `sparsity`, `engine` and `n_rows` and the outcome was
# `log_fold` which is defined as
# `log(time to fit with dense data / time to fit with sparse data)`.
# Meaning that values above above 0 would reflects longer fit times for dense,
# Hence we want to use sparse data.
#
# At this time the only engines that support sparse data are glmnet, LiblineaR,
# ranger, and xgboost. Which is why they are the only ones listed here.
# This is fine as this code will only run if `allow_sparse()` returns `TRUE`
# Which only happens for these engines.
#
# Ranger is hard-coded to always fail since they appear to use the same
# algorithm for sparse and dense data, resulting in identical times.
should_use_sparsity <- function(sparsity, engine, n_rows) {
  if (is.null(engine) || engine == "ranger") {
    return("no")
  }

  log_fold <- -0.599333138645995 +
    ifelse(sparsity < 0.836601307189543, 0.836601307189543 - sparsity, 0) *
      -0.541581853008009 +
    ifelse(n_rows < 16000, 16000 - n_rows, 0) * 3.23980908942813e-05 +
    ifelse(n_rows > 16000, n_rows - 16000, 0) * -2.81001152147355e-06 +
    ifelse(sparsity > 0.836601307189543, sparsity - 0.836601307189543, 0) *
      9.82444255114058 +
    ifelse(sparsity > 0.836601307189543, sparsity - 0.836601307189543, 0) *
      ifelse(n_rows > 8000, n_rows - 8000, 0) *
      7.27456967763306e-05 +
    ifelse(sparsity > 0.836601307189543, sparsity - 0.836601307189543, 0) *
      ifelse(n_rows < 8000, 8000 - n_rows, 0) *
      -0.000798307404212627

  if (engine == "xgboost") {
    log_fold <- log_fold +
      ifelse(sparsity < 0.984615384615385, 0.984615384615385 - sparsity, 0) *
        0.113098025073806 +
      ifelse(n_rows < 8000, 8000 - n_rows, 0) * -9.77914237255269e-05 +
      ifelse(n_rows > 8000, n_rows - 8000, 0) * 3.22657666511869e-06 +
      ifelse(sparsity > 0.984615384615385, sparsity - 0.984615384615385, 0) *
        41.5180348086939 +
      0.913457808326756
  }

  if (engine == "LiblineaR") {
    log_fold <- log_fold +
      ifelse(sparsity > 0.836601307189543, sparsity - 0.836601307189543, 0) *
        -5.39592564852111
  }

  ifelse(log_fold > 0, "yes", "no")
}
