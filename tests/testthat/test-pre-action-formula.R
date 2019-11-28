test_that("can add a formula to a workflow", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_is(workflow$pre$actions$formula, "action_formula")
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
  workflow0 <- workflow()
  workflow  <- add_formula(workflow0, mpg ~ cyl)
  workflow  <- remove_formula(workflow)
  expect_equal(workflow0$pre, workflow$pre)
})

test_that("remove a formula after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow0 <- workflow()
  workflow0 <- add_model(workflow0, lm_model)
  workflow  <- add_formula(workflow0, mpg ~ cyl)

  workflow <- fit(workflow, data = mtcars)
  workflow <- remove_formula(workflow)
  expect_equal(workflow0$pre, workflow$pre)
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
  workflow <- update_formula(workflow, mpg ~ disp)
  expect_equal(workflow$pre$actions$formula$formula, mpg ~ disp)
  expect_null(workflow$pre$mold)
})
