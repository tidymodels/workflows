test_that("can predict from a workflow", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, mtcars)

  result <- predict(fit_workflow, mtcars)

  expect_is(result, "tbl_df")
  expect_equal(nrow(result), 32)
})

test_that("workflow must have been `fit()` before prediction can be done", {
  expect_error(predict(workflow(), mtcars), "Workflow has not yet been trained")
})

test_that("formula preprocessing is done to the `new_data`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ log(cyl))
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, mtcars)

  result1 <- predict(fit_workflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, mtcars_with_log)

  result2 <- predict(fit_workflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("recipe preprocessing is done to the `new_data`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec <- recipes::step_log(rec, cyl)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, mtcars)

  result1 <- predict(fit_workflow, mtcars)

  # pre-log the data
  mtcars_with_log <- mtcars
  mtcars_with_log$cyl <- log(mtcars_with_log$cyl)

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, mtcars_with_log)

  result2 <- predict(fit_workflow, mtcars_with_log)

  expect_equal(result1, result2)
})

test_that("`new_data` must have all of the original predictors", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec <- recipes::step_log(rec, cyl)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, mtcars)

  cars_no_cyl <- mtcars
  cars_no_cyl$cyl <- NULL

  expect_error(predict(fit_workflow, cars_no_cyl), "missing: 'cyl'")
})

test_that("blueprint will get passed on to hardhat::forge()", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  # Pass formula explicitly to keep `lm()` from auto-generating an intercept
  workflow <- workflow()
  workflow <- add_model(workflow, mod, formula = mpg ~ . + 0)

  blueprint_no_intercept <- hardhat::default_formula_blueprint(intercept = FALSE)
  workflow_no_intercept <- add_formula(workflow, mpg ~ hp + disp, blueprint = blueprint_no_intercept)
  fit_no_intercept <- fit(workflow_no_intercept, mtcars)
  prediction_no_intercept <- predict(fit_no_intercept, mtcars)

  blueprint_with_intercept <- hardhat::default_formula_blueprint(intercept = TRUE)
  workflow_with_intercept <- add_formula(workflow, mpg ~ hp + disp, blueprint = blueprint_with_intercept)
  fit_with_intercept <- fit(workflow_with_intercept, mtcars)
  prediction_with_intercept <- predict(fit_with_intercept, mtcars)

  expect_false(fit_no_intercept$pre$mold$blueprint$intercept)
  expect_true(fit_with_intercept$pre$mold$blueprint$intercept)

  expect_false(identical(prediction_with_intercept, prediction_no_intercept))
})
