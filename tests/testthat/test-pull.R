skip_if_not_installed("recipes")

# ------------------------------------------------------------------------------
# pull_workflow_preprocessor()

test_that("can pull a formula preprocessor", {
  local_lifecycle_quiet()

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_equal(
    pull_workflow_preprocessor(workflow),
    mpg ~ cyl
  )
})

test_that("can pull a recipe preprocessor", {
  local_lifecycle_quiet()

  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_equal(
    pull_workflow_preprocessor(workflow),
    recipe
  )
})

test_that("can pull a variables preprocessor", {
  local_lifecycle_quiet()

  variables <- workflow_variables(mpg, c(cyl, disp))

  workflow <- workflow()
  workflow <- add_variables(workflow, variables = variables)

  expect_identical(
    pull_workflow_preprocessor(workflow),
    variables
  )
})

test_that("error if no preprocessor", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_preprocessor(workflow()))
})

test_that("error if not a workflow", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_preprocessor(1))
})

test_that("`pull_workflow_preprocessor()` is soft-deprecated", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_snapshot(x <- pull_workflow_preprocessor(workflow))
})

# ------------------------------------------------------------------------------
# pull_workflow_spec()

test_that("can pull a model spec", {
  local_lifecycle_quiet()

  model <- parsnip::linear_reg()

  workflow <- workflow()
  workflow <- add_model(workflow, model)

  expect_equal(
    pull_workflow_spec(workflow),
    model
  )
})

test_that("error if no spec", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_spec(workflow()))
})

test_that("error if not a workflow", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_spec(1))
})

test_that("`pull_workflow_spec()` is soft-deprecated", {
  model <- parsnip::linear_reg()

  workflow <- workflow()
  workflow <- add_model(workflow, model)

  expect_snapshot(x <- pull_workflow_spec(workflow))
})

# ------------------------------------------------------------------------------
# pull_workflow_fit()

test_that("can pull a model fit", {
  local_lifecycle_quiet()

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_equal(
    pull_workflow_fit(workflow),
    workflow$fit$fit
  )
})

test_that("error if no fit", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_fit(workflow()))
})

test_that("error if not a workflow", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_fit(1))
})

test_that("`pull_workflow_fit()` is soft-deprecated", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_snapshot(x <- pull_workflow_fit(workflow))
})

# ------------------------------------------------------------------------------
# pull_workflow_mold()

test_that("can pull a mold", {
  local_lifecycle_quiet()

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_type(pull_workflow_mold(workflow), "list")

  expect_equal(
    pull_workflow_mold(workflow),
    workflow$pre$mold
  )
})

test_that("error if no mold", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_mold(workflow()))
})

test_that("error if not a workflow", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_mold(1))
})

test_that("`pull_workflow_mold()` is soft-deprecated", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_snapshot(x <- pull_workflow_mold(workflow))
})

# ------------------------------------------------------------------------------
# pull_workflow_prepped_recipe()

test_that("can pull a prepped recipe", {
  local_lifecycle_quiet()

  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_recipe(workflow, recipe)

  workflow <- fit(workflow, mtcars)

  expect_s3_class(pull_workflow_prepped_recipe(workflow), "recipe")

  expect_equal(
    pull_workflow_prepped_recipe(workflow),
    workflow$pre$mold$blueprint$recipe
  )
})

test_that("error if no recipe preprocessor", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_prepped_recipe(workflow()))
})

test_that("error if no mold", {
  local_lifecycle_quiet()

  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_snapshot(error = TRUE, pull_workflow_prepped_recipe(workflow))
})

test_that("error if not a workflow", {
  local_lifecycle_quiet()

  expect_snapshot(error = TRUE, pull_workflow_prepped_recipe(1))
})

test_that("`pull_workflow_prepped_recipe()` is soft-deprecated", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_recipe(workflow, recipe)

  workflow <- fit(workflow, mtcars)

  expect_snapshot(x <- pull_workflow_prepped_recipe(workflow))
})
