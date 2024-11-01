skip_if_not_installed("recipes")

# ------------------------------------------------------------------------------
# workflow()

test_that("can create a basic workflow", {
  workflow <- workflow()

  expect_s3_class(workflow, "workflow")

  expect_s3_class(workflow$pre, "stage_pre")
  expect_s3_class(workflow$fit, "stage_fit")
  expect_s3_class(workflow$post, "stage_post")

  expect_equal(workflow$pre$actions, new_named_list())
  expect_equal(workflow$pre$mold, NULL)

  expect_equal(workflow$fit$actions, new_named_list())
  expect_equal(workflow$fit$fit, NULL)

  expect_equal(workflow$post$actions, new_named_list())
})

test_that("workflow must be the first argument when adding actions", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  mod <- parsnip::linear_reg()

  expect_snapshot(error = TRUE, add_formula(1, mpg ~ cyl))
  expect_snapshot(error = TRUE, add_recipe(1, rec))
  expect_snapshot(error = TRUE, add_model(1, mod))
})

test_that("can add a model spec directly to a workflow", {
  mod <- parsnip::linear_reg()
  workflow <- workflow(spec = mod)

  expect_identical(workflow$fit$actions$model$spec, mod)
})

test_that("can add a preprocessor directly to a workflow", {
  preprocessor <- recipes::recipe(mpg ~ cyl, mtcars)
  workflow <- workflow(preprocessor)
  expect_identical(workflow$pre$actions$recipe$recipe, preprocessor)

  preprocessor <- mpg ~ cyl
  workflow <- workflow(preprocessor)
  expect_identical(workflow$pre$actions$formula$formula, preprocessor)

  preprocessor <- workflow_variables(mpg, cyl)
  workflow <- workflow(preprocessor)
  expect_identical(workflow$pre$actions$variables$variables, preprocessor)
})

test_that("model spec is validated", {
  expect_snapshot(error = TRUE, workflow(spec = 1))
})

test_that("preprocessor is validated", {
  expect_snapshot(error = TRUE, workflow(preprocessor = 1))
})

# ------------------------------------------------------------------------------
# new_workflow()

test_that("constructor validates input", {
  expect_snapshot(error = TRUE, new_workflow(pre = 1))
  expect_snapshot(error = TRUE, new_workflow(fit = 1))
  expect_snapshot(error = TRUE, new_workflow(post = 1))

  expect_snapshot(error = TRUE, new_workflow(trained = 1))
})

# ------------------------------------------------------------------------------
# is_trained_workflow()

test_that("can check if a workflow is trained", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  wf <- workflow()
  wf <- add_recipe(wf, rec)
  wf <- add_model(wf, mod)

  expect_false(is_trained_workflow(wf))
  wf <- fit(wf, mtcars)
  expect_true(is_trained_workflow(wf))
})

test_that("input must be a workflow", {
  expect_snapshot_error(is_trained_workflow(1))
})
