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
  expect_error(predict(workflow(), mtcars), "Workflow has not yet been fit")
})

test_that("can pass `type` through to parsnip", {
  mod <- parsnip::multinom_reg(penalty = 0.01)
  mod <- parsnip::set_engine(mod, "glmnet")

  workflow <- workflow()
  workflow <- add_formula(workflow, Species ~ Sepal.Length + Sepal.Width)
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, iris)

  result <- predict(fit_workflow, iris)
  expect_equal(names(result), ".pred_class")

  # TODO https://github.com/tidymodels/parsnip/issues/234
  # result <- predict(fit_workflow, iris, type = "prob")
  # expect_equal(names(result), ".pred_...)
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
