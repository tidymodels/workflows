test_that("can add a recipe to a workflow", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)

  expect_is(workflow$pre$actions$recipe, "action_recipe")
})

test_that("recipe is validated", {
  expect_error(add_recipe(workflow(), 1), "`recipe` must be a recipe")
})

test_that("cannot add a recipe if a formula already exists", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_error(add_recipe(workflow, rec), "cannot be added when a formula already exists")
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

  expect_error(add_recipe(workflow, rec), "`recipe` action has already been added")
})

