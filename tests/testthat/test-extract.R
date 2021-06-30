# ------------------------------------------------------------------------------
# extract_preprocessor()

test_that("can extract a formula preprocessor", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_equal(
    extract_preprocessor(workflow),
    mpg ~ cyl
  )
})

test_that("can extract a recipe preprocessor", {
  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_equal(
    extract_preprocessor(workflow),
    recipe
  )
})

test_that("can extract a variables preprocessor", {
  variables <- workflow_variables(mpg, c(cyl, disp))

  workflow <- workflow()
  workflow <- add_variables(workflow, variables = variables)

  expect_identical(
    extract_preprocessor(workflow),
    variables
  )
})

test_that("error if no preprocessor", {
  expect_error(
    extract_preprocessor(workflow()),
    "does not have a preprocessor"
  )
})

test_that("error if not a workflow", {
  expect_error(
    extract_preprocessor(1),
    "no applicable method"
  )
})

# ------------------------------------------------------------------------------
# extract_spec_parsnip()

test_that("can extract a model spec", {
  model <- parsnip::linear_reg()

  workflow <- workflow()
  workflow <- add_model(workflow, model)

  expect_equal(
    extract_spec_parsnip(workflow),
    model
  )
})

test_that("error if no spec", {
  expect_error(
    extract_spec_parsnip(workflow()),
    "does not have a model spec"
  )
})

test_that("error if not a workflow", {
  expect_error(
    extract_spec_parsnip(1),
    "no applicable method for"
  )
})

# ------------------------------------------------------------------------------
# extract_fit_parsnip()

test_that("can extract a parsnip model fit", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_equal(
    extract_fit_parsnip(workflow),
    workflow$fit$fit
  )
})

test_that("error if no parsnip fit", {
  expect_error(
    extract_fit_parsnip(workflow()),
    "does not have a model fit. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    extract_fit_parsnip(1),
    "no applicable method for"
  )
})

# ------------------------------------------------------------------------------
# extract_fit_engine()

test_that("can extract a engine model fit", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_equal(
    extract_fit_engine(workflow),
    workflow$fit$fit$fit
  )
})

# ------------------------------------------------------------------------------
# extract_mold()

test_that("can extract a mold", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, mtcars)

  expect_type(extract_mold(workflow), "list")

  expect_equal(
    extract_mold(workflow),
    workflow$pre$mold
  )
})

test_that("error if no mold", {
  expect_error(
    extract_mold(workflow()),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    extract_mold(1),
    "no applicable method"
  )
})

# ------------------------------------------------------------------------------
# extract_recipe()

test_that("can extract a prepped recipe", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_model(workflow, model)
  workflow <- add_recipe(workflow, recipe)

  workflow <- fit(workflow, mtcars)

  expect_s3_class(extract_recipe(workflow), "recipe")

  expect_equal(
    extract_recipe(workflow),
    workflow$pre$mold$blueprint$recipe
  )

  expect_snapshot(error = TRUE, extract_recipe(workflow, FALSE))

  expect_error(
    extract_recipe(workflow, estimated = "yes please"),
    "`estimated` must be a single `TRUE` or `FALSE`"
  )
})

test_that("error if no recipe preprocessor", {
  expect_error(
    extract_recipe(workflow()),
    "must have a recipe preprocessor"
  )
})

test_that("error if no mold", {
  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_error(
    extract_recipe(workflow),
    "does not have a mold. Have you called `fit[(][)]` yet?"
  )
})

test_that("error if not a workflow", {
  expect_error(
    extract_recipe(1),
    "no applicable method for"
  )
})
