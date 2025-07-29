skip_if_not_installed("recipes")

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
  expect_snapshot(error = TRUE, butcher::butcher(workflow()))
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

test_that("can axe the `$template` in a recipe preprocessor through `axe_fitted()` (#147)", {
  skip_if_not_installed("butcher")

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  rec <- recipes::recipe(mpg ~ cyl + disp, mtcars)
  rec <- recipes::step_center(rec, cyl, disp)

  wf <- workflow()
  wf <- add_model(wf, model)
  wf <- add_recipe(wf, rec)

  fit <- fit(wf, mtcars)
  fit <- butcher::axe_fitted(fit)

  # `axe_fitted()` is run on the un-fitted recipe to remove the `$template`
  expect_identical(
    fit$pre$actions$recipe$recipe$template,
    vctrs::vec_ptype(rec$template)
  )

  # `hardhat:::compost()` removed the template from the fitted recipe when
  # `hardhat::mold()` was run
  expect_null(fit$pre$mold$blueprint$recipe$template)
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

  original_outcomes <- fit$pre$actions$variables$variables$outcomes
  original_predictors <- fit$pre$actions$variables$variables$predictors

  axed_outcomes <- axed$pre$actions$variables$variables$outcomes
  axed_predictors <- axed$pre$actions$variables$variables$predictors

  expect_s3_class(axed_outcomes, "quosure")
  expect_s3_class(axed_predictors, "quosure")

  expect_false(identical(original_outcomes, axed_outcomes))
  expect_false(identical(original_predictors, axed_predictors))
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
