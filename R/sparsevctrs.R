is_sparse_matrix <- function(x) {
  methods::is(x, "sparseMatrix")
}

toggle_sparsity <- function(object, data) {
  toggle_sparse <- "no"

  if (allow_sparse(expect_spec_parsnip(object))) {
    if (has_preprocessor_recipe(object)) {
      est_sparsity <- recipes::.recipes_estimate_sparsity(
        extract_preprocessor(object)
      )
    } else {
      est_sparsity <- sparsevctrs::sparsity(data, sample = 1000)
    }

    pred_log_fold <- pred_log_fold(
      est_sparsity,
      extract_spec_parsnip(object)$engine,
      nrow(data)
    )
    if (pred_log_fold > 0) {
      toggle_sparse <- "yes"
    }
  }

  object$pre$actions$recipe$recipe <- recipes::.recipes_toggle_sparse_args(
    object$pre$actions$recipe$recipe,
    choice = toggle_sparse
  )
  object
}

allow_sparse <- function(x) {
  if (inherits(x, "model_fit")) {
    x <- x$spec
  }
  res <- parsnip::get_from_env(paste0(class(x)[1], "_encoding"))
  all(res$allow_sparse_x[res$engine == x$engine])
}

pred_log_fold <- function(sparsity, model, n_rows) {
  if (is.null(model) || model == "ranger") {
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

  if (model == "xgboost") {
    log_fold <- log_fold +
      ifelse(sparsity < 0.984615384615385, 0.984615384615385 - sparsity, 0) *
        0.113098025073806 +
      ifelse(n_rows < 8000, 8000 - n_rows, 0) * -9.77914237255269e-05 +
      ifelse(n_rows > 8000, n_rows - 8000, 0) * 3.22657666511869e-06 +
      ifelse(sparsity > 0.984615384615385, sparsity - 0.984615384615385, 0) *
        41.5180348086939 +
      0.913457808326756
  }

  if (model == "LiblineaR") {
    log_fold <- log_fold +
      ifelse(sparsity > 0.836601307189543, sparsity - 0.836601307189543, 0) *
        -5.39592564852111
  }

  log_fold
}
