test_that("can add a formula to a workflow", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_s3_class(workflow$pre$actions$formula, "action_formula")
})

test_that("formula is validated", {
  expect_error(add_formula(workflow(), 1), "`formula` must be a formula")
})

test_that("cannot add a formula if a recipe already exists", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)

  expect_error(add_formula(workflow, mpg ~ cyl), "cannot be added when a recipe already exists")
})

test_that("cannot add a formula if variables already exist", {
  workflow <- workflow()
  workflow <- add_variables(workflow, y, x)

  expect_error(add_formula(workflow, mpg ~ cyl), "cannot be added when variables already exist")
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

  expect_error(add_formula(workflow, mpg ~ cyl), "`formula` action has already been added")
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

  workflow_with_formula  <- add_formula(workflow_no_formula, mpg ~ cyl)
  workflow_with_formula <- fit(workflow_with_formula, data = mtcars)

  workflow_removed_formula <- remove_formula(workflow_with_formula)

  expect_equal(workflow_no_formula$pre, workflow_removed_formula$pre)
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

test_that("can only use a 'formula_blueprint' blueprint", {
  blueprint <- hardhat::default_recipe_blueprint()

  workflow <- workflow()

  expect_error(
    add_formula(workflow, mpg ~ cyl, blueprint = blueprint),
    "must be a hardhat 'formula_blueprint'"
  )
})
