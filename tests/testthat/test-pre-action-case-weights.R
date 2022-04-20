# ------------------------------------------------------------------------------
# add_case_weights()

test_that("case weights + formula uses weights in the model", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, y ~ x)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(wf$fit$fit$fit$weights, 1L)
})

test_that("case weights + variables uses weights in the model", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_variables(wf, y, x)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(wf$fit$fit$fit$weights, 1L)
})

test_that("case weights + recipe uses weights in the model", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(y ~ x + w, df)
  # Step that might use case weights
  rec <- recipes::step_center(rec, x)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(wf$fit$fit$fit$weights, 1L)

  expect_named(wf$pre$mold$outcomes, "y")
  expect_named(wf$pre$mold$predictors, "x")
  expect_named(wf$pre$mold$extras$roles$case_weights, "w")
})

test_that("case weights are used with model formula override", {
  df <- vctrs::data_frame(y = 1, x = 2, z = 3, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(y ~ x + z + w, df)
  # Step that might use case weights
  rec <- recipes::step_center(rec, x)

  wf <- workflow()
  wf <- add_model(wf, spec, formula = y ~ x)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(wf$fit$fit$fit$weights, 1L)

  # Importantly, supplying a model formula that uses `~ .` doesn't include weights
  wf <- update_model(wf, spec, formula = y ~ .)
  wf <- fit(wf, df)
  expect_named(wf$fit$fit$fit$coefficients, c("(Intercept)", "x", "z"))
})

test_that("case weights + formula doesn't need case weights at predict time", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 2),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, y ~ x)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  df$y <- NULL
  df$w <- NULL

  expect_equal(
    predict(wf, df)$.pred,
    c(1/6, 2, 1/6)
  )
})

test_that("case weights + variables doesn't need case weights at predict time", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 2),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_variables(wf, y, x)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  df$y <- NULL
  df$w <- NULL

  expect_equal(
    predict(wf, df)$.pred,
    c(1/6, 2, 1/6)
  )
})

test_that("case weights + recipe doesn't need case weights at predict time", {
  skip("Until hardhat ignores case weight roles at predict time by default")

  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 2),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(y ~ x + w, df)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  df$y <- NULL
  df$w <- NULL

  expect_equal(
    predict(wf, df)$.pred,
    c(1/6, 2, 1/6)
  )
})

test_that("case weights + formula removes weights before formula evaluation", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, y ~ .)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(wf$pre$case_weights, df$w)
  expect_false("w" %in% names(wf$pre$mold$predictors))
})

test_that("case weights + variables removes weights before variables evaluation", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_variables(wf, y, everything())
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(wf$pre$case_weights, df$w)
  expect_false("w" %in% names(wf$pre$mold$predictors))
})

test_that("case weights + recipe retains weights for use in recipe", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(y ~ x + w, df)
  # Step that might use case weights
  rec <- recipes::step_center(rec, x)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  # recipe won't run unless the `w` column is there
  wf <- fit(wf, df)

  expect_identical(wf$pre$case_weights, df$w)
  expect_identical(wf$pre$mold$extras$roles$case_weights$w, df$w)
})

test_that("case weights added after preprocessors get reordered", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  rec <- recipes::recipe(y ~ x + w, df)

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_case_weights(wf, w)

  # Order matters
  expect_identical(names(wf$pre$actions), c("case_weights", "formula"))

  wf <- workflow()
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  # Order matters
  expect_identical(names(wf$pre$actions), c("case_weights", "recipe"))

  wf <- workflow()
  wf <- add_variables(wf, y, x)
  wf <- add_case_weights(wf, w)

  # Order matters
  expect_identical(names(wf$pre$actions), c("case_weights", "variables"))
})

test_that("case weights `col` must exist in `data`", {
  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, mpg ~ .)
  wf <- add_case_weights(wf, foo)

  # Tidyselect error
  expect_error(fit(wf, mtcars))
})

test_that("case weights `col` can't select >1 columns in `data`", {
  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, mpg ~ .)
  wf <- add_case_weights(wf, c(cyl, disp))

  expect_snapshot(error = TRUE, fit(wf, mtcars))
})

test_that("case weights must inherit from the base case weights class", {
  df <- vctrs::data_frame(y = 1, x = 1, weights = 1)

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, y ~ x)
  wf <- add_case_weights(wf, weights)

  expect_snapshot(error = TRUE, fit(wf, df))
})

# ------------------------------------------------------------------------------
# remove_case_weights()

test_that("can remove case weights (and keep preprocessor)", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  wf <- workflow()
  wf <- add_formula(wf, y ~ .)
  wf <- add_case_weights(wf, w)

  wf <- remove_case_weights(wf)

  expect_identical(names(wf$pre$actions), "formula")
})

test_that("removing case weights resets model, mold, and case-weights slots", {
  df <- vctrs::data_frame(y = 1, x = 2, w = hardhat::frequency_weights(1))

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, y ~ .)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  wf <- remove_case_weights(wf)

  expect_null(wf$pre$mold)
  expect_null(wf$pre$case_weights)
  expect_null(wf$fit$fit)
})

# ------------------------------------------------------------------------------
# update_case_weights()

test_that("updating case weights resets model, mold, and case-weights slots", {
  df <- vctrs::data_frame(
    y = 1,
    x = 2,
    w = hardhat::frequency_weights(1),
    z = hardhat::frequency_weights(2)
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, y ~ x)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  wf <- update_case_weights(wf, z)

  expect_null(wf$pre$mold)
  expect_null(wf$pre$case_weights)
  expect_null(wf$fit$fit)
  expect_identical(wf$pre$actions$case_weights$col, quo(z))
})
