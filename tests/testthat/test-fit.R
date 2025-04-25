skip_if_not_installed("recipes")

data("hardhat-example-data", package = "hardhat")

test_that("can `fit()` a workflow with a recipe", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  result <- fit(workflow, mtcars)

  expect_s3_class(result$fit$fit, "model_fit")

  expect_equal(
    coef(result$fit$fit$fit),
    coef(lm(formula = mpg ~ cyl, data = mtcars))
  )
})

test_that("can `fit()` a workflow with a formula", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  result <- fit(workflow, mtcars)

  expect_s3_class(result$fit$fit, "model_fit")

  expect_equal(
    coef(result$fit$fit$fit),
    coef(lm(formula = mpg ~ cyl, data = mtcars))
  )
})

test_that("missing `data` argument has a nice error", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  expect_snapshot(error = TRUE, fit(workflow))
})

test_that("invalid `control` argument has a nice error", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  control <- parsnip::control_parsnip()

  expect_snapshot(error = TRUE, {
    fit(workflow, mtcars, control = control)
  })
})

test_that("cannot fit without a pre stage", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, mod)

  expect_snapshot(error = TRUE, {
    fit(workflow, mtcars)
  })
})

test_that("cannot fit without a fit stage", {
  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)

  expect_snapshot(error = TRUE, {
    fit(workflow, mtcars)
  })
})

test_that("fit.workflow confirms compatibility of object and calibration", {
  skip_if_not_installed("tailor")
  skip_if_not_installed("probably")

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  expect_snapshot(
    res <- fit(workflow, mtcars, calibration = mtcars)
  )

  tailor <- tailor::tailor()
  tailor <- tailor::adjust_numeric_calibration(tailor)
  workflow <- add_tailor(workflow, tailor)

  expect_snapshot(error = TRUE, {
    fit(workflow, mtcars)
  })
})

# ------------------------------------------------------------------------------
# .fit_pre()

test_that("`.fit_pre()` updates a formula blueprint according to parsnip's encoding info", {
  workflow <- workflow()
  workflow <- add_formula(workflow, num_1 ~ .)

  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, example_train)

  # ranger sets `indicators = 'none'`, so `Species` is not expanded
  expected <- "fac_1"
  expect_true(expected %in% names(result$pre$mold$predictors))
  expect_identical(result$pre$actions$formula$blueprint$indicators, "none")

  mod <- parsnip::boost_tree(trees = 5)
  mod <- parsnip::set_engine(mod, "xgboost")
  mod <- parsnip::set_mode(mod, "regression")
  workflow <- update_model(workflow, mod)

  result <- .fit_pre(workflow, example_train)

  # xgboost sets `indicators = 'one_hot'`, so `Species` is expanded to three values
  expected <- c("fac_1a", "fac_1b", "fac_1c")
  expect_true(all(expected %in% names(result$pre$mold$predictors)))
  expect_identical(result$pre$actions$formula$blueprint$indicators, "one_hot")
})

test_that("`.fit_pre()` ignores parsnip's encoding info with recipes", {
  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")
  rec <- recipes::recipe(num_1 ~ ., example_train)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, example_train)

  # recipe preprocessing won't auto-expand factors
  expect_true("fac_1" %in% names(result$pre$mold$predictors))
  expect_false("indicators" %in% names(result$pre$actions$recipe$blueprint))
})

test_that("`.fit_pre()` doesn't modify user supplied formula blueprint", {
  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")

  # request `indicators` to be used, even though parsnip's info on ranger
  # says not to make them.
  blueprint <- hardhat::default_formula_blueprint(indicators = "traditional")

  workflow <- workflow()
  workflow <- add_formula(workflow, num_1 ~ ., blueprint = blueprint)
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, example_train)

  expected <- c("fac_1a", "fac_1b", "fac_1c")
  expect_true(all(expected %in% names(result$pre$mold$predictors)))
  expect_identical(result$pre$actions$formula$blueprint, blueprint)
})

test_that("`.fit_pre()` doesn't modify user supplied recipe blueprint", {
  mod <- parsnip::rand_forest()
  mod <- parsnip::set_engine(mod, "ranger")
  mod <- parsnip::set_mode(mod, "regression")
  rec <- recipes::recipe(num_1 ~ ., example_train)

  blueprint <- hardhat::default_recipe_blueprint(allow_novel_levels = TRUE)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec, blueprint = blueprint)
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, example_train)

  expect_true("fac_1" %in% names(result$pre$mold$predictors))
  expect_identical(result$pre$actions$recipe$blueprint, blueprint)
})

# ------------------------------------------------------------------------------
# .fit_post()
test_that(".workflow_includes_calibration works", {
  skip_if_not_installed("tailor")
  skip_if_not_installed("probably")

  expect_false(.workflow_includes_calibration(workflow()))
  expect_false(.workflow_includes_calibration(
    workflow() |> add_model(parsnip::linear_reg())
  ))
  expect_false(.workflow_includes_calibration(
    workflow() |> add_formula(mpg ~ .)
  ))
  expect_false(.workflow_includes_calibration(
    workflow() |>
      add_formula(mpg ~ .) |>
      add_model(parsnip::linear_reg())
  ))
  expect_false(.workflow_includes_calibration(
    workflow() |>
      add_tailor(tailor::tailor())
  ))
  expect_false(.workflow_includes_calibration(
    workflow() |>
      add_tailor(tailor::tailor() |> tailor::adjust_probability_threshold(.4))
  ))

  expect_true(.workflow_includes_calibration(
    workflow() |>
      add_tailor(tailor::tailor() |> tailor::adjust_numeric_calibration())
  ))
  expect_true(.workflow_includes_calibration(
    workflow() |>
      add_tailor(
        tailor::tailor() |>
          tailor::adjust_numeric_calibration() |>
          tailor::adjust_numeric_range(lower_limit = 1)
      )
  ))
  expect_true(.workflow_includes_calibration(
    workflow() |>
      add_formula(mpg ~ .) |>
      add_model(parsnip::linear_reg()) |>
      add_tailor(
        tailor::tailor() |>
          tailor::adjust_numeric_calibration() |>
          tailor::adjust_numeric_range(lower_limit = 1)
      )
  ))
})

# ------------------------------------------------------------------------------
# .fit_finalize()

test_that("workflow is marked as 'trained' after going through `.fit_finalize()`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  workflow_pre <- .fit_pre(workflow, mtcars)
  workflow_model <- .fit_model(workflow_pre, control_workflow())
  workflow_final <- .fit_finalize(workflow_model)

  expect_false(is_trained_workflow(workflow_model))
  expect_true(is_trained_workflow(workflow_final))
})

test_that("can `predict()` from workflow fit from individual pieces", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  workflow_pre <- .fit_pre(workflow, mtcars)
  workflow_model <- .fit_model(workflow_pre, control_workflow())
  workflow_final <- .fit_finalize(workflow_model)

  workflow_fit <- fit(workflow, mtcars)
  expect <- predict(workflow_fit, mtcars)

  expect_snapshot(error = TRUE, predict(workflow_model, mtcars))
  expect_identical(predict(workflow_final, mtcars), expect)
})
