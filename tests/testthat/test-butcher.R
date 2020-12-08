test_that("attaches the butcher class", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_formula(wf, mpg ~ cyl + disp)

  fit <- fit(wf, mtcars)
  fit <- butcher::butcher(fit)

  expect_s3_class(fit, "butchered_workflow")
})

test_that("fails if not a fitted workflow", {
  skip_if_not_installed("butcher")
  expect_error(butcher::butcher(workflow()))
})

test_that("can axe the call", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_formula(wf, mpg ~ cyl + disp)

  fit <- fit(wf, mtcars)
  fit <- butcher::axe_call(fit)

  expect_identical(fit$fit$fit$fit$call, rlang::expr(dummy_call()))
})

test_that("can axe the fitted bits", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_formula(wf, mpg ~ cyl + disp)

  fit <- fit(wf, mtcars)
  fit <- butcher::axe_fitted(fit)

  expect_identical(fit$fit$fit$fit$fitted.values, numeric())
})

test_that("axing the data removes the outcomes/predictors", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_formula(wf, mpg ~ cyl + disp)

  fit <- fit(wf, mtcars)

  expect_s3_class(fit$pre$mold$outcomes, "tbl_df")
  expect_s3_class(fit$pre$mold$predictors, "tbl_df")

  fit <- butcher::axe_data(fit)

  expect_null(fit$pre$mold$outcomes)
  expect_null(fit$pre$mold$predictors)
})

test_that("axing the env - recipe", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  rec <- recipes::recipe(mpg ~ cyl + disp, mtcars)
  rec <- recipes::step_center(rec, cyl, disp)

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_recipe(wf, rec)

  fit <- fit(wf, mtcars)
  fit <- butcher::axe_env(fit)

  expect_s3_class(fit$pre$actions$recipe$recipe, "butchered_recipe")
})

test_that("axing the env - formula", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_formula(wf, mpg ~ cyl + disp)

  fit <- fit(wf, mtcars)
  fit <- butcher::axe_env(fit)

  expect_s3_class(fit$pre$actions$formula$formula, "butchered_formula")
})

test_that("axing the env - variables", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_variables(wf, mpg, c(cyl, disp))

  fit <- fit(wf, mtcars)
  axed <- butcher::axe_env(fit)

  expect_s3_class(axed, "butchered_workflow")
  expect_false(identical(fit$pre$actions$variables$outcomes, axed$pre$actions$variables$outcomes))
  expect_false(identical(fit$pre$actions$variables$predictors, axed$pre$actions$variables$predictors))
})

test_that("can still predict after butcher - recipe", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  rec <- recipes::recipe(mpg ~ cyl + disp, mtcars)
  rec <- recipes::step_center(rec, cyl, disp)

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_recipe(wf, rec)

  fit <- fit(wf, mtcars)
  axed <- butcher::axe_env(fit)

  expect_identical(
    predict(fit, mtcars),
    predict(axed, mtcars)
  )
})

test_that("can still predict after butcher - formula", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_formula(wf, mpg ~ cyl + disp)

  fit <- fit(wf, mtcars)
  axed <- butcher::axe_env(fit)

  expect_identical(
    predict(fit, mtcars),
    predict(axed, mtcars)
  )
})

test_that("can still predict after butcher - variables", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_variables(wf, mpg, c(cyl, disp))

  fit <- fit(wf, mtcars)
  axed <- butcher::axe_env(fit)

  expect_identical(
    predict(fit, mtcars),
    predict(axed, mtcars)
  )
})
