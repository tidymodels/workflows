test_that("can create a basic workflow", {
  workflow <- workflow()

  expect_is(workflow, "workflow")

  expect_is(workflow$pre, "stage_pre")
  expect_is(workflow$fit, "stage_fit")
  expect_is(workflow$post, "stage_post")

  expect_equal(workflow$pre$actions, list())
  expect_equal(workflow$pre$mold, NULL)

  expect_equal(workflow$fit$actions, list())
  expect_equal(workflow$fit$fit, NULL)

  expect_equal(workflow$post$actions, list())
})

test_that("workflow must be the first argument when adding actions", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  mod <- parsnip::linear_reg()

  expect_error(add_formula(1, mpg ~ cyl), "must be a workflow")
  expect_error(add_recipe(1, rec), "must be a workflow")
  expect_error(add_model(1, mod), "must be a workflow")
})

test_that("constructor validates input", {
  expect_error(new_workflow(pre = 1), "must be a `stage`")
  expect_error(new_workflow(fit = 1), "must be a `stage`")
  expect_error(new_workflow(post = 1), "must be a `stage`")

  expect_error(new_workflow(run = 1), "must be a single logical value")
})

# ------------------------------------------------------------------------------

test_that("can print empty workflow", {
  verify_output(
    test_path("out/test-print-workflow-empty.txt"),
    workflow()
  )
})

test_that("can print workflow with recipe", {
  rec <- recipes::recipe(mtcars)

  verify_output(
    test_path("out/test-print-workflow-recipe.txt"),
    add_recipe(workflow(), rec)
  )
})

test_that("can print workflow with formula", {
  verify_output(
    test_path("out/test-print-workflow-formula.txt"),
    add_formula(workflow(), y ~ x)
  )
})

test_that("can print workflow with model", {
  model <- parsnip::linear_reg()

  verify_output(
    test_path("out/test-print-workflow-model.txt"),
    add_model(workflow(), model)
  )
})

test_that("can print workflow with model with engine specific args", {
  model <- parsnip::linear_reg(penalty = 0.01)
  model <- parsnip::set_engine(model, "glmnet", dfmax = 5)

  verify_output(
    test_path("out/test-print-workflow-model-args.txt"),
    add_model(workflow(), model)
  )
})

test_that("can print workflow with fit model", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, model)

  verify_output(
    test_path("out/test-print-workflow-fit.txt"),
    fit(workflow, mtcars)
  )
})
