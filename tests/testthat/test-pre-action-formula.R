skip_if_not_installed("recipes")

test_that("can add a formula to a workflow", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_s3_class(workflow$pre$actions$formula, "action_formula")
})

test_that("formula is validated", {
  expect_snapshot(error = TRUE, add_formula(workflow(), 1))
})

test_that("cannot add a formula if a recipe already exists", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)

  expect_snapshot(error = TRUE, add_formula(workflow, mpg ~ cyl))
})

test_that("cannot add a formula if variables already exist", {
  workflow <- workflow()
  workflow <- add_variables(workflow, y, x)

  expect_snapshot(error = TRUE, add_formula(workflow, mpg ~ cyl))
})

test_that("formula preprocessing is executed upon `fit()`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ log(cyl))
  workflow <- add_model(workflow, mod)

  result <- fit(workflow, mtcars)

  expect_equal(
    result$pre$mold$outcomes$mpg,
    mtcars$mpg
  )

  expect_equal(
    result$pre$mold$predictors$`log(cyl)`,
    log(mtcars$cyl)
  )
})

test_that("cannot add two formulas", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_snapshot(error = TRUE, add_formula(workflow, mpg ~ cyl))
})

test_that("remove a formula", {
  workflow_no_formula <- workflow()
  workflow_with_formula <- add_formula(workflow_no_formula, mpg ~ cyl)
  workflow_removed_formula <- remove_formula(workflow_with_formula)

  expect_equal(workflow_no_formula$pre, workflow_removed_formula$pre)
})

test_that("remove a formula after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow_no_formula <- workflow()
  workflow_no_formula <- add_model(workflow_no_formula, lm_model)

  workflow_with_formula <- add_formula(workflow_no_formula, mpg ~ cyl)
  workflow_with_formula <- fit(workflow_with_formula, data = mtcars)

  workflow_removed_formula <- remove_formula(workflow_with_formula)

  expect_equal(workflow_no_formula$pre, workflow_removed_formula$pre)
})

test_that("removing a formula doesn't remove case weights", {
  wf <- workflow()
  wf <- add_formula(wf, mpg ~ .)
  wf <- add_case_weights(wf, disp)

  wf <- remove_formula(wf)

  expect_identical(names(wf$pre$actions), "case_weights")
})

test_that("update a formula", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- update_formula(workflow, mpg ~ disp)

  expect_equal(workflow$pre$actions$formula$formula, mpg ~ disp)
})

test_that("update a formula after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, lm_model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, data = mtcars)

  # Should clear fitted model
  workflow <- update_formula(workflow, mpg ~ disp)

  expect_equal(workflow$pre$actions$formula$formula, mpg ~ disp)

  expect_equal(workflow$fit$actions$model$spec, lm_model)
  expect_null(workflow$pre$mold)
})

test_that("can pass a blueprint through to hardhat::mold()", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  blueprint <- hardhat::default_formula_blueprint(intercept = TRUE)

  workflow <- workflow()
  workflow <- add_model(workflow, lm_model)
  workflow <- add_formula(workflow, mpg ~ cyl, blueprint = blueprint)

  workflow <- fit(workflow, data = mtcars)

  expect_true("(Intercept)" %in% colnames(workflow$pre$mold$predictors))
  expect_equal(workflow$pre$actions$formula$blueprint, blueprint)
  expect_true(workflow$pre$mold$blueprint$intercept)
})

test_that("can't pass an `offset()` through `add_formula()` (#162)", {
  df <- vctrs::data_frame(
    y = c(1.5, 2.5, 3.5, 1, 3),
    x = c(2, 6, 7, 3, 6),
    o = c(1.1, 2, 3, .5, 2)
  )

  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, lm_model)
  workflow <- add_formula(workflow, y ~ x + offset(o))

  expect_snapshot(error = TRUE, {
    fit(workflow, data = df)
  })
})

test_that("can only use a 'formula_blueprint' blueprint", {
  blueprint <- hardhat::default_recipe_blueprint()

  workflow <- workflow()

  expect_snapshot(
    error = TRUE,
    add_formula(workflow, mpg ~ cyl, blueprint = blueprint)
  )
})
