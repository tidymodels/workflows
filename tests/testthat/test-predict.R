skip_if_not_installed("recipes")

test_that("can predict from a workflow", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  fit_workflow <- fit(workflow, mtcars)

  result <- predict(fit_workflow, mtcars)

  expect_s3_class(result, "tbl_df")
  expect_equal(nrow(result), 32)
})

test_that("workflow must have been `fit()` before prediction can be done", {
  expect_snapshot(error = TRUE, predict(workflow(), mtcars))
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

  # This error comes from hardhat, so we don't snapshot it
  expect_error(predict(fit_workflow, cars_no_cyl))
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

  bp1 <- hardhat::default_formula_blueprint(
    intercept = TRUE,
    allow_novel_levels = FALSE
  )
  bp2 <- hardhat::default_formula_blueprint(
    intercept = TRUE,
    allow_novel_levels = TRUE
  )

  workflow <- workflow()
  workflow <- add_model(workflow, spec)

  workflow1 <- add_formula(workflow, y ~ x, blueprint = bp1)
  workflow2 <- add_formula(workflow, y ~ x, blueprint = bp2)

  mod1 <- fit(workflow1, train)
  mod2 <- fit(workflow2, train)

  # Warning from hardhat, so we don't snapshot it
  expect_warning(pred1 <- predict(mod1, test))
  expect_no_warning(pred2 <- predict(mod2, test))

  expect_identical(
    pred1[[".pred"]],
    c(2, 4.5, NA)
  )

  expect_identical(
    pred2[[".pred"]],
    c(2, 4.5, 2)
  )
})

test_that("monitoring: no double intercept due to dot expansion in model formula #210", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  # model formula includes a dot to mean "everything available after the preprocessing formula
  workflow <- workflow()
  workflow <- add_model(workflow, mod, formula = mpg ~ .)

  blueprint_with_intercept <- hardhat::default_formula_blueprint(
    intercept = TRUE
  )
  workflow_with_intercept <- add_formula(
    workflow,
    mpg ~ hp + disp,
    blueprint = blueprint_with_intercept
  )
  fit_with_intercept <- fit(workflow_with_intercept, mtcars)

  # The dot expansion used to include the intercept column, added via the blueprint, as a regular predictor.
  # `parsnip:::prepare_data()` removed this column, so lm's predict method errored.
  # Now it gets removed before fitting (lm will handle the intercept itself),
  # so lm()'s predict method won't error anymore here. (tidymodels/parsnip#1033)
  expect_no_error(predict(fit_with_intercept, mtcars))
})

test_that("predict(type) is respected with a postprocessor (#251)", {
  skip_if_not_installed("tailor")
  # create example data
  y <- seq(0, 7, .1)
  d <- data.frame(
    y = as.factor(ifelse(y > 3.5, "yes", "no")),
    x = y + (y - 3)^2
  )
  wflow <- workflow(y ~ ., parsnip::logistic_reg(), tailor::tailor())
  wflow_fit <- fit(wflow, d)

  pred_class <- predict(wflow_fit, d[1:5, ], type = "class")
  pred_prob <- predict(wflow_fit, d[1:5, ], type = "prob")
  pred_null <- predict(wflow_fit, d[1:5, ])

  expect_named(pred_class, ".pred_class")
  expect_named(pred_prob, c(".pred_no", ".pred_yes"), ignore.order = TRUE)
  expect_equal(pred_class, pred_null)

  expect_snapshot(error = TRUE, predict(wflow_fit, d[1:5, ], type = "boop"))
})
