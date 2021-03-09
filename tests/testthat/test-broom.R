# ------------------------------------------------------------------------------
# tidy()

test_that("can't tidy the model of an unfit workflow", {
  x <- workflow()
  expect_error(tidy(x), "does not have a model fit")
})

test_that("can't tidy the recipe of an unfit workflow", {
  x <- workflow()

  expect_error(tidy(x, what = "recipe"), "must have a recipe preprocessor")

  rec <- recipes::recipe(y ~ x, data.frame(y = 1, x = 1))

  x <- add_recipe(x, rec)

  expect_error(tidy(x, what = "recipe"), "does not have a mold")
})

test_that("can tidy workflow model or recipe", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 3))

  rec <- recipes::recipe(y ~ x, df)
  rec <- recipes::step_log(rec, x)

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_recipe(wf, rec)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  x <- tidy(wf)
  expect_identical(x$term, c("(Intercept)", "x"))

  x <- tidy(wf, what = "recipe")
  expect_identical(x$number, 1L)
})

# ------------------------------------------------------------------------------
# glance()

test_that("can't glance at the model of an unfit workflow", {
  x <- workflow()
  expect_snapshot_error(glance(x))
})

test_that("can glance at a fitted workflow's model", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 3))

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  x <- glance(wf)

  expect_s3_class(x, "tbl_df")
  expect_identical(nrow(x), 1L)
})

# ------------------------------------------------------------------------------
# augment()

test_that("can't augment with the model of an unfit workflow", {
  x <- workflow()
  x <- add_model(x, parsnip::linear_reg())
  expect_snapshot_error(augment(x, mtcars))
})

test_that("can augment using a fitted workflow's model", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 3))

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  x <- augment(wf, df)

  expect_s3_class(x, "tbl_df")
  expect_identical(nrow(x), 3L)

  # at least 1 prediction specific column should be added
  expect_true(ncol(x) > ncol(df))
})

test_that("augment returns `new_data`, not the pre-processed version of `new_data`", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = factor(c("a", "b", "a")))

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  # Returns `new_data` + prediction columns
  x <- augment(wf, df)

  expect_true(all(names(df) %in% names(x)))
})

test_that("augment fails if it can't preprocess `new_data`", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = factor(c("a", "b", "a")))
  new_data <- data.frame(y = c(2, 3, 4), x = 1:3)

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  expect_error(augment(wf, new_data), class = "vctrs_error_incompatible_type")
})
