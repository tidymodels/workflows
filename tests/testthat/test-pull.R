# ------------------------------------------------------------------------------
# pull_workflow_preprocessor()

test_that("can pull a formula preprocessor", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_equal(
    pull_workflow_preprocessor(workflow),
    mpg ~ cyl
  )
})

test_that("can pull a recipe preprocessor", {
  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_equal(
    pull_workflow_preprocessor(workflow),
    recipe
  )
})

test_that("error if no preprocessor", {
  expect_error(
    pull_workflow_preprocessor(workflow()),
    "does not have a preprocessor"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_preprocessor(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_workflow_spec()

test_that("can pull a model spec", {
  model <- parsnip::linear_reg()

  workflow <- workflow()
  workflow <- add_model(workflow, model)

  expect_equal(
    pull_workflow_spec(workflow),
    model
  )
})

test_that("error if no spec", {
  expect_error(
    pull_workflow_spec(workflow()),
    "does not have a model spec"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_spec(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_workflow_fit()

test_that("can pull a model fit", {
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
  expect_error(
    pull_workflow_fit(workflow()),
    "does not have a model fit. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_fit(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_workflow_mold()

test_that("can pull a mold", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_is(pull_workflow_mold(workflow), "list")

  expect_equal(
    pull_workflow_mold(workflow),
    workflow$pre$mold
  )
})

test_that("error if no mold", {
  expect_error(
    pull_workflow_mold(workflow()),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_mold(1),
    "must be a workflow"
  )
})

# ------------------------------------------------------------------------------
# pull_workflow_prepped_recipe()

test_that("can pull a prepped recipe", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_recipe(workflow, recipe)

  workflow <- fit(workflow, mtcars)

  expect_is(pull_workflow_prepped_recipe(workflow), "recipe")

  expect_equal(
    pull_workflow_prepped_recipe(workflow),
    workflow$pre$mold$blueprint$recipe
  )
})

test_that("error if no recipe preprocessor", {
  expect_error(
    pull_workflow_prepped_recipe(workflow()),
    "must have a recipe preprocessor"
  )
})

test_that("error if no mold", {
  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_error(
    pull_workflow_prepped_recipe(workflow),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    pull_workflow_prepped_recipe(1),
    "must be a workflow"
  )
})
