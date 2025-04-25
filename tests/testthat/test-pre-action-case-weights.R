skip_if_not_installed("recipes")

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
  df <- vctrs::data_frame(
    y = 1,
    x = 2,
    z = 3,
    w = hardhat::frequency_weights(1)
  )

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
    c(1 / 6, 2, 1 / 6)
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
    c(1 / 6, 2, 1 / 6)
  )
})

test_that("case weights + recipe doesn't need case weights at predict time", {
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
    c(1 / 6, 2, 1 / 6)
  )
})

test_that("case weights + recipe can optionally require case weights at predict time", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 2),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(y ~ x + w, df)
  rec <- recipes::update_role_requirements(rec, "case_weights", bake = TRUE)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(
    wf$pre$mold$extras$roles$case_weights$w,
    df$w
  )

  df$y <- NULL

  expect_equal(
    predict(wf, df)$.pred,
    c(1 / 6, 2, 1 / 6)
  )

  df$w <- NULL

  # missing case weights error--don't snapshot test so as not to be
  # sensitive to the wording of the error message (#278)
  skip_on_cran()
  expect_error(predict(wf, df), regexp = "missing")
})

test_that("case weights + recipe requires extra roles at predict time by default", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 2),
    w = c(2, 5, 10)
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  # In this recipe, `w` is used as some `id` role that is actually required at
  # `bake()` time
  rec <- recipes::recipe(y ~ x + w, df)
  rec <- recipes::update_role(rec, w, new_role = "id")
  rec <- recipes::step_center(rec, w)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)

  wf <- fit(wf, df)

  expect_identical(
    wf$pre$mold$extras$roles$id$w,
    df$w - mean(df$w)
  )

  df$y <- NULL

  expect_equal(
    predict(wf, df)$.pred,
    c(1 / 2, 2, 1 / 2)
  )

  df$w <- NULL

  # missing case weights error--don't snapshot test so as not to be
  # sensitive to the wording of the error message (#278)
  skip_on_cran()
  expect_error(predict(wf, df), regexp = "missing")
})

test_that("case weights + recipe can optionally not require extra roles at predict time", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 2),
    w = c(2, 5, 10)
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  # In this recipe, `w` is used as some `id` role that isn't actually needed
  # at `bake()` time
  rec <- recipes::recipe(y ~ x + w, df)
  rec <- recipes::update_role(rec, w, new_role = "id")
  rec <- recipes::update_role_requirements(rec, "id", bake = FALSE)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)

  wf <- fit(wf, df)

  expect_identical(
    wf$pre$mold$extras$roles$id$w,
    df$w
  )

  df$y <- NULL

  expect_equal(
    predict(wf, df)$.pred,
    c(1 / 2, 2, 1 / 2)
  )

  df$w <- NULL

  # Works without `w`
  expect_equal(
    predict(wf, df)$.pred,
    c(1 / 2, 2, 1 / 2)
  )
})

test_that("case weights + recipe updates the case weights if the recipe filters rows", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 3),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  # The filter drops one of the rows, so the case weights need to be updated accordingly
  rec <- recipes::recipe(y ~ x + w, df)
  rec <- recipes::step_filter(rec, x > 2)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  wf <- fit(wf, df)

  expect_identical(
    wf$pre$case_weights,
    df$w[c(2, 3)]
  )
  expect_identical(
    wf$pre$mold$extras$roles$case_weights$w,
    df$w[c(2, 3)]
  )
})

test_that("case weights + recipe doesn't allow the recipe to drop the case weights column", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 3),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(y ~ x + w, df)
  rec <- recipes::step_mutate(rec, w = NULL)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  expect_snapshot(error = TRUE, {
    fit(wf, df)
  })
})

test_that("case weights + recipe doesn't allow the recipe to adjust the case weights column class", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 3),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(y ~ x + w, df)
  rec <- recipes::step_mutate(rec, w = unclass(w))

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  expect_snapshot(error = TRUE, {
    fit(wf, df)
  })
})

test_that("case weights + recipe doesn't allow the recipe to change the name of the case weights column", {
  df <- vctrs::data_frame(
    y = c(1, 2, 0),
    x = c(2, 5, 3),
    w = hardhat::frequency_weights(c(2, 5, 10))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  # Adds a new case weights column, but removes the old one
  rec <- recipes::recipe(y ~ x + w, df)
  rec <- recipes::step_mutate(rec, w2 = w, w = NULL, role = "case_weights")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, w)

  expect_snapshot(error = TRUE, {
    fit(wf, df)
  })
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

  expect_error(fit(wf, mtcars), class = "vctrs_error_subscript_oob")
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
