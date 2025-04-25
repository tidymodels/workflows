skip_if_not_installed("recipes")

test_that("can add a recipe to a workflow", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)

  expect_s3_class(workflow$pre$actions$recipe, "action_recipe")
})

test_that("recipe is validated", {
  expect_snapshot(error = TRUE, add_recipe(workflow(), 1))
})

test_that("cannot add a recipe if a formula already exists", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_snapshot(error = TRUE, add_recipe(workflow, rec))
})

test_that("cannot add a recipe if variables already exist", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_variables(workflow, y, x)

  expect_snapshot(error = TRUE, add_recipe(workflow, rec))
})

test_that("cannot add a recipe if recipe is trained", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars) |> recipes::prep()

  workflow <- workflow()

  expect_snapshot(error = TRUE, add_recipe(workflow, rec))
})

test_that("remove a recipe", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow_no_recipe <- workflow()
  workflow_with_recipe <- add_recipe(workflow_no_recipe, rec)
  workflow_removed_recipe <- remove_recipe(workflow_with_recipe)

  expect_equal(workflow_no_recipe$pre, workflow_removed_recipe$pre)
})

test_that("remove a recipe after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow_no_recipe <- workflow()
  workflow_no_recipe <- add_model(workflow_no_recipe, lm_model)

  workflow_with_recipe <- add_recipe(workflow_no_recipe, rec)
  workflow_with_recipe <- fit(workflow_with_recipe, data = mtcars)

  workflow_removed_recipe <- remove_recipe(workflow_with_recipe)

  expect_equal(workflow_no_recipe$pre, workflow_removed_recipe$pre)
})

test_that("removing a formula doesn't remove case weights", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  wf <- workflow()
  wf <- add_recipe(wf, rec)
  wf <- add_case_weights(wf, disp)

  wf <- remove_recipe(wf)

  expect_identical(names(wf$pre$actions), "case_weights")
})

test_that("update a recipe", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec2 <- recipes::recipe(mpg ~ disp, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- update_recipe(workflow, rec2)

  expect_equal(workflow$pre$actions$recipe$recipe, rec2)
})

test_that("update a recipe after model fit", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec2 <- recipes::recipe(mpg ~ disp, mtcars)

  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, lm_model)
  workflow <- add_recipe(workflow, rec)

  workflow <- fit(workflow, data = mtcars)

  # Should clear fitted model
  workflow <- update_recipe(workflow, rec2)

  expect_equal(workflow$pre$actions$recipe$recipe, rec2)

  expect_equal(workflow$fit$actions$model$spec, lm_model)
  expect_null(workflow$pre$mold)
})

test_that("recipe is prepped upon `fit()`", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec <- recipes::step_center(rec, cyl)

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  result <- fit(workflow, mtcars)

  expect_equal(
    result$pre$mold$outcomes$mpg,
    mtcars$mpg
  )

  expect_equal(
    result$pre$mold$predictors$cyl,
    mtcars$cyl - mean(mtcars$cyl)
  )

  center_step <- result$pre$mold$blueprint$recipe$steps[[1]]

  expect_true(recipes::is_trained(center_step))
})

test_that("cannot add two recipe", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)

  expect_snapshot(error = TRUE, add_recipe(workflow, rec))
})

test_that("can pass a blueprint through to hardhat::mold()", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  blueprint <- hardhat::default_recipe_blueprint(intercept = TRUE)

  workflow <- workflow()
  workflow <- add_model(workflow, lm_model)
  workflow <- add_recipe(workflow, rec, blueprint = blueprint)

  workflow <- fit(workflow, data = mtcars)

  expect_true("(Intercept)" %in% colnames(workflow$pre$mold$predictors))
  expect_equal(workflow$pre$actions$recipe$blueprint, blueprint)
  expect_true(workflow$pre$mold$blueprint$intercept)
})

test_that("can only use a 'recipe_blueprint' blueprint", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  blueprint <- hardhat::default_formula_blueprint()

  workflow <- workflow()

  expect_snapshot(
    error = TRUE,
    add_recipe(workflow, rec, blueprint = blueprint)
  )
})
