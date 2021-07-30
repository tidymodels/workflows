# ------------------------------------------------------------------------------
# add_case_weights()

test_that("case weights + formula removes weights before formula evaluation", {
  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, mpg ~ .)
  wf <- add_case_weights(wf, disp)

  wf <- fit(wf, mtcars)

  expect_identical(wf$pre$case_weights, mtcars$disp)
  expect_false("disp" %in% names(wf$pre$mold$predictors))
})

test_that("case weights + variables removes weights before variables evaluation", {
  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_variables(wf, mpg, everything())
  wf <- add_case_weights(wf, disp)

  wf <- fit(wf, mtcars)

  expect_identical(wf$pre$case_weights, mtcars$disp)
  expect_false("disp" %in% names(wf$pre$mold$predictors))
})

test_that("case weights can be retained for usage in a recipe with `remove = FALSE`", {
  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  rec <- recipes::recipe(mpg ~ cyl + disp, mtcars)
  # Mock step that might use case weights
  rec <- recipes::step_center(rec, cyl, disp)

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, disp, remove = FALSE)

  wf <- fit(wf, mtcars)

  expect_identical(wf$pre$case_weights, mtcars$disp)
  expect_true("disp" %in% names(wf$pre$mold$predictors))
})

test_that("case weights added after preprocessors get reordered", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  wf <- workflow()
  wf <- add_formula(wf, mpg ~ cyl)
  wf <- add_case_weights(wf, disp)

  # Order matters
  expect_identical(names(wf$pre$actions), c("case_weights", "formula"))

  wf <- workflow()
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, disp)

  # Order matters
  expect_identical(names(wf$pre$actions), c("case_weights", "recipe"))

  wf <- workflow()
  wf <- add_variables(wf, mpg, cyl)
  wf <- add_case_weights(wf, disp)

  # Order matters
  expect_identical(names(wf$pre$actions), c("case_weights", "variables"))
})

test_that("`remove` is validated", {
  expect_snapshot(error = TRUE, add_case_weights(workflow(), foo, remove = 1))
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

# ------------------------------------------------------------------------------
# remove_case_weights()

test_that("can remove case weights (and keep preprocessor)", {
  wf <- workflow()
  wf <- add_formula(wf, mpg ~ .)
  wf <- add_case_weights(wf, disp)

  wf <- remove_case_weights(wf)

  expect_identical(names(wf$pre$actions), "formula")
})

test_that("removing case weights resets model, mold, and case-weights slots", {
  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, mpg ~ .)
  wf <- add_case_weights(wf, disp)

  wf <- fit(wf, mtcars)

  wf <- remove_case_weights(wf)

  expect_null(wf$pre$mold)
  expect_null(wf$pre$case_weights)
  expect_null(wf$fit$fit)
})

# ------------------------------------------------------------------------------
# update_case_weights()

test_that("updating case weights resets model, mold, and case-weights slots", {
  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  wf <- workflow()
  wf <- add_model(wf, spec)
  wf <- add_formula(wf, mpg ~ .)
  wf <- add_case_weights(wf, disp)

  wf <- fit(wf, mtcars)

  wf <- update_case_weights(wf, cyl, remove = FALSE)

  expect_null(wf$pre$mold)
  expect_null(wf$pre$case_weights)
  expect_null(wf$fit$fit)
  expect_false(wf$pre$actions$case_weights$remove)
})
