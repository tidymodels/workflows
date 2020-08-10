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
  train <- data.frame(
    y = c(1L, 5L, 3L, 4L),
    x = factor(c("x", "y", "x", "y"))
  )

  test <- data.frame(
    x = factor(c("x", "y", "z"))
  )

  spec <- parsnip::linear_reg()
  spec <- parsnip::set_engine(spec, "lm")

  bp1 <- hardhat::default_formula_blueprint(intercept = TRUE, allow_novel_levels = FALSE)
  bp2 <- hardhat::default_formula_blueprint(intercept = TRUE, allow_novel_levels = TRUE)

  workflow <- workflow()
  workflow <- add_model(workflow, spec)

  workflow1 <- add_formula(workflow, y ~ x, blueprint = bp1)
  workflow2 <- add_formula(workflow, y ~ x, blueprint = bp2)

  mod1 <- fit(workflow1, train)
  mod2 <- fit(workflow2, train)

  expect_warning(pred1 <- predict(mod1, test))
  expect_warning(pred2 <- predict(mod2, test), NA)

  expect_identical(
    pred1[[".pred"]],
    c(2, 4.5, NA)
  )

  expect_identical(
    pred2[[".pred"]],
    c(2, 4.5, 2)
  )
})

test_that("monitoring: known that parsnip removes blueprint intercept for some models (tidymodels/parsnip#353)", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  # Pass formula explicitly to keep `lm()` from auto-generating an intercept
  workflow <- workflow()
  workflow <- add_model(workflow, mod, formula = mpg ~ . + 0)

  blueprint_with_intercept <- hardhat::default_formula_blueprint(intercept = TRUE)
  workflow_with_intercept <- add_formula(workflow, mpg ~ hp + disp, blueprint = blueprint_with_intercept)
  fit_with_intercept <- fit(workflow_with_intercept, mtcars)

  # `parsnip:::prepare_data()` will remove the intercept, so it won't be
  # there when the `lm()` `predict()` method is called.
  expect_error(predict(fit_with_intercept, mtcars))
})
