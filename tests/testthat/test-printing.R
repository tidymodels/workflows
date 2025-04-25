skip_if_not_installed("recipes")

test_that("can print empty workflow", {
  expect_snapshot(workflow())
})

test_that("can print workflow with recipe", {
  rec <- recipes::recipe(mtcars)
  expect_snapshot(add_recipe(workflow(), rec))
})

test_that("can print workflow with formula", {
  expect_snapshot(add_formula(workflow(), y ~ x))
})

test_that("can print workflow with variables", {
  expect_snapshot(add_variables(workflow(), y, c(x1, x2)))
})

test_that("can print workflow with model", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  expect_snapshot(add_model(workflow(), model))
})

test_that("can print workflow with model with engine specific args", {
  model <- parsnip::linear_reg(penalty = 0.01)
  model <- parsnip::set_engine(model, "glmnet", dfmax = 5)

  expect_snapshot(add_model(workflow(), model))
})

test_that("can print workflow with fit model", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, model)

  expect_snapshot(fit(workflow, mtcars))
})

test_that("can print workflow with >10 recipe steps", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)

  expect_snapshot(add_recipe(workflow(), rec))

  rec <- recipes::step_log(rec, cyl)

  expect_snapshot(add_recipe(workflow(), rec))
})

test_that("can print workflow with just case weights", {
  workflow <- workflow()
  workflow <- add_case_weights(workflow, disp)

  expect_snapshot(workflow)
})

test_that("can print workflow with case weights, preprocessor, and model", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ .)
  workflow <- add_case_weights(workflow, disp)
  workflow <- add_model(workflow, model)

  expect_snapshot(workflow)
})

test_that("can print workflow with postprocessor", {
  skip_if_not_installed("tailor")

  post <- tailor::tailor()
  workflow <- workflow()
  workflow <- add_postprocessor(workflow, post)

  expect_snapshot(workflow)
})
