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
