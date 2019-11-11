test_that("can add a model to a workflow", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, mod)

  expect_is(workflow$fit$actions$model, "action_model")
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
