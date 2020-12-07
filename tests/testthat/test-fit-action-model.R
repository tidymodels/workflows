test_that("can add a model to a workflow", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, mod)

  expect_s3_class(workflow$fit$actions$model, "action_model")
})

test_that("model is validated", {
  expect_error(add_model(workflow(), 1), "`spec` must be a `model_spec`")
})

test_that("cannot add two models", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, mod)

  expect_error(add_model(workflow, mod), "`model` action has already been added")
})

test_that("can provide a model formula override", {
  # disp is in the recipe, but excluded from the model formula
  rec <- recipes::recipe(mpg ~ cyl + disp, mtcars)
  rec <- recipes::step_center(rec, cyl)

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod, formula = mpg ~ cyl)

  result <- fit(workflow, mtcars)

  expect_equal(
    c("(Intercept)", "cyl"),
    names(result$fit$fit$fit$coefficients)
  )
})


test_that("remove a model", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow_no_model <- workflow()
  workflow_no_model <- add_formula(workflow_no_model, mpg ~ cyl)

  workflow_with_model  <- add_model(workflow_no_model, lm_model)
  workflow_removed_model  <- remove_model(workflow_with_model)

  expect_equal(workflow_no_model$fit, workflow_removed_model$fit)
})

test_that("remove a model after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  workflow_no_model <- workflow()
  workflow_no_model <- add_formula(workflow_no_model, mpg ~ cyl)

  workflow_with_model  <- add_model(workflow_no_model, lm_model)
  workflow_with_model <- fit(workflow_with_model, data = mtcars)

  workflow_removed_model  <- remove_model(workflow_with_model)

  expect_equal(workflow_no_model$fit, workflow_removed_model$fit)
})

test_that("update a model", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  glmn_model <- parsnip::set_engine(lm_model, "glmnet")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, lm_model)
  workflow <- update_model(workflow, glmn_model)

  expect_equal(workflow$fit$actions$model$spec$engine, "glmnet")
})


test_that("update a model after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")
  no_model <- parsnip::set_engine(lm_model, "lm", model = FALSE)

  workflow <- workflow()
  workflow <- add_model(workflow, no_model)
  workflow <- add_formula(workflow, mpg ~ cyl)

  workflow <- fit(workflow, data = mtcars)
  workflow <- update_model(workflow, lm_model)

  # Should no longer have `model = FALSE` engine arg
  engine_args <- workflow$fit$actions$model$spec$eng_args
  expect_false(any(names(engine_args) == "model"))

  # The fitted model should be removed
  expect_null(workflow$fit$fit)
})
