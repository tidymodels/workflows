test_that("basic functionality", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  library(parsnip)

  silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
    seq(1, 2, length.out = length(.propensity))
  }

  lr_fit <- fit(workflow(Class ~ A + B, logistic_reg()), two_class_dat)

  lr_res1 <- weight_propensity(lr_fit, silly_wt_fn, data = two_class_dat)
  expect_s3_class(lr_res1, "tbl_df")
  expect_true(all(names(lr_res1) %in% c(names(two_class_dat), ".wts")))
  expect_equal(lr_res1$.wts, importance_weights(seq(1, 2, length.out = nrow(two_class_dat))))
})

test_that("errors informatively with bad input", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  library(parsnip)

  silly_wt_fn <- function(.propensity, .exposure = NULL, ...) {
    seq(1, 2, length.out = length(.propensity))
  }

  # untrained workflow
  wf <- workflow(Class ~ A + B, logistic_reg())

  expect_snapshot(
    error = TRUE,
    weight_propensity(wf, silly_wt_fn, data = two_class_dat)
  )

  # bad `wt_fn`
  wf_fit <- fit(wf, two_class_dat)

  expect_snapshot(
    error = TRUE,
    weight_propensity(wf_fit, data = two_class_dat)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(wf_fit, "boop", data = two_class_dat)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(wf_fit, function(...) {-1L}, data = two_class_dat)
  )

  # bad `data`
  expect_snapshot(
    error = TRUE,
    weight_propensity(wf_fit, silly_wt_fn)
  )

  expect_snapshot(
    error = TRUE,
    weight_propensity(wf_fit, silly_wt_fn, data = "boop")
  )
})
