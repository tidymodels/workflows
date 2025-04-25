skip_if_not_installed("probably")
skip_if_not_installed("tailor")

test_that("can add a postprocessor to a workflow", {
  post <- tailor::tailor()

  workflow <- workflow()
  workflow <- add_tailor(workflow, post)

  expect_s3_class(workflow$post$actions$tailor, "action_tailor")
})

test_that("postprocessor is validated", {
  expect_snapshot(error = TRUE, add_tailor(workflow(), 1))
})

test_that("cannot add two postprocessors", {
  post <- tailor::tailor()

  workflow <- workflow()
  workflow <- add_tailor(workflow, post)

  expect_snapshot(error = TRUE, add_tailor(workflow, post))
})

test_that("remove a postprocessor", {
  post <- tailor::tailor()

  workflow_no_post <- workflow()
  workflow_no_post <- add_formula(workflow_no_post, mpg ~ cyl)

  workflow_with_post <- add_tailor(workflow_no_post, post)
  workflow_removed_post <- remove_tailor(workflow_with_post)

  expect_equal(workflow_no_post$post, workflow_removed_post$post)
})

test_that("remove a postprocessor after postprocessor fit", {
  post <- tailor::tailor()

  workflow_no_post <- workflow()
  workflow_no_post <- add_formula(workflow_no_post, mpg ~ cyl)
  workflow_no_post <- add_model(workflow_no_post, parsnip::linear_reg())

  workflow_with_post <- add_tailor(workflow_no_post, post)
  workflow_with_post <- fit(workflow_with_post, data = mtcars)

  workflow_removed_post <- remove_tailor(workflow_with_post)

  expect_equal(workflow_no_post$post, workflow_removed_post$post)
})

test_that("update a postprocessor", {
  post <- tailor::tailor()
  post2 <- tailor::adjust_numeric_range(post, 0, Inf)

  workflow <- workflow()
  workflow <- add_tailor(workflow, post)
  workflow <- update_tailor(workflow, post2)

  expect_length(workflow$post$actions$tailor$tailor$adjustments, 1)
})

test_that("update a postprocessor after postprocessor fit", {
  post <- tailor::tailor()
  post2 <- tailor::adjust_numeric_range(post, 0, Inf)

  workflow_no_post <- workflow()
  workflow_no_post <- add_formula(workflow_no_post, mpg ~ cyl)
  workflow_no_post <- add_model(workflow_no_post, parsnip::linear_reg())

  workflow_with_post <- add_tailor(workflow_no_post, post)
  workflow_with_post <- fit(workflow_with_post, data = mtcars)

  workflow_with_post_new <- update_tailor(workflow_with_post, post2)

  expect_length(
    workflow_with_post_new$post$actions$tailor$tailor$adjustments,
    1
  )

  # Note that the fitted model and preprocessor can remain; the new
  # postprocessor will not affect it (#225)
  expect_equal(workflow_with_post$fit, workflow_with_post_new$fit)
})

test_that("postprocessor fit aligns with manually fitted version (no calibration)", {
  skip_if_not_installed("modeldata")

  # create example data
  y <- seq(0, 7, .1)
  dat <- data.frame(y = y, x = y + (y - 3)^2)

  # construct workflows
  post <- tailor::tailor()
  post <- tailor::adjust_numeric_range(post, 0, 5)

  wflow_simple <- workflow(y ~ ., parsnip::linear_reg())
  wflow_post <- add_tailor(wflow_simple, post)

  # train workflow
  wf_simple_fit <- fit(wflow_simple, dat)
  wf_post_fit <- fit(wflow_post, dat)

  # ...verify predictions are the same as training the post-proc separately
  wflow_simple_preds <- augment(wf_simple_fit, dat)
  post_trained <- fit(post, wflow_simple_preds, y, .pred)
  wflow_manual_preds <- predict(post_trained, wflow_simple_preds)

  wflow_post_preds <- predict(wf_post_fit, dat)

  expect_equal(wflow_manual_preds[".pred"], wflow_post_preds)
  expect_false(all(wflow_simple_preds[".pred"] == wflow_manual_preds[".pred"]))
})

test_that("postprocessor fit aligns with manually fitted version (with calibration)", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("mgcv")

  # create example data
  y <- seq(0, 7, .1)
  dat <- data.frame(y = y, x = y + (y - 3)^2)

  dat_data <- dat[1:40, ]
  dat_calibration <- dat[41:71, ]

  # construct workflows
  post <- tailor::tailor()
  post <- tailor::adjust_numeric_calibration(post, "linear")

  wflow_simple <- workflow(y ~ ., parsnip::linear_reg())
  wflow_post <- add_tailor(wflow_simple, post)

  # train workflows
  wf_simple_fit <- fit(wflow_simple, dat_data)
  wf_post_fit <- fit(wflow_post, dat_data, calibration = dat_calibration)

  # ...verify predictions are the same as training the post-proc separately.
  # note that this test naughtily re-predicts on the calibration set.
  wflow_simple_preds <- augment(wf_simple_fit, dat_calibration)
  post_trained <- fit(post, wflow_simple_preds, y, .pred)
  wflow_manual_preds <- predict(post_trained, wflow_simple_preds)

  wflow_post_preds <- predict(wf_post_fit, dat_calibration)

  expect_equal(wflow_manual_preds[".pred"], wflow_post_preds)

  # okay if some predictions are the same, but we wouldn't expect all of them to be
  expect_false(all(wflow_simple_preds[".pred"] == wflow_manual_preds[".pred"]))
})
