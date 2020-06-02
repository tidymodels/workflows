test_that("can `.fit_pre()` a workflow with a formula", {
  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")

  workflow <- workflow()
  workflow <- add_formula(workflow, Sepal.Length ~ .)
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, iris)

  expect_is(result$fit, "stage_fit")
  expect_equal(ncol(result$pre$mold$predictors), 4)
  expect_false(result$pre$actions$formula$blueprint$indicators)
})


test_that("can `.fit_pre()` a workflow with a recipe", {
  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")
  rec <- recipes::recipe(Sepal.Length ~ ., iris)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, iris)

  expect_is(result$fit, "stage_fit")
  expect_equal(ncol(result$pre$mold$predictors), 4)
  expect_false("indicators" %in% names(result$pre$actions$recipe$blueprint))
})

test_that("can `.fit_pre()` with user supplied formula blueprint", {
  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")

  workflow <- workflow()
  workflow <- add_formula(
    workflow, Sepal.Length ~ .,
    blueprint = hardhat::default_formula_blueprint(indicators = TRUE)
  )
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, iris)

  expect_is(result$fit, "stage_fit")
  expect_equal(ncol(result$pre$mold$predictors), 6)
  expect_true(result$pre$actions$formula$blueprint$indicators)
})

test_that("can `.fit_pre()` with user supplied recipe blueprint", {
  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")
  rec <- recipes::recipe(Sepal.Length ~ ., iris)

  workflow <- workflow()
  workflow <- add_recipe(
    workflow, rec,
    blueprint = hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)
  )
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, iris)

  expect_is(result$fit, "stage_fit")
  expect_equal(ncol(result$pre$mold$predictors), 4)
  expect_false("indicators" %in% names(result$pre$actions$recipe$blueprint))
})
