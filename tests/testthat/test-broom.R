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
