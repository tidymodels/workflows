skip_if_not_installed("recipes")

test_that("can add variables to a workflow", {
  wf <- workflow()
  wf <- add_variables(wf, y, c(x1, x2))

  expect_s3_class(wf$pre$actions$variables, "action_variables")
  expect_s3_class(wf$pre$actions$variables$variables, "workflow_variables")
  expect_identical(wf$pre$actions$variables$variables$outcomes, quo(y))
  expect_identical(
    wf$pre$actions$variables$variables$predictors,
    quo(c(x1, x2))
  )
})

test_that("can add variables to a workflow with `variables` specification", {
  wf <- workflow()
  wf <- add_variables(wf, variables = workflow_variables(y, c(x1, x2)))

  expect_s3_class(wf$pre$actions$variables, "action_variables")
  expect_s3_class(wf$pre$actions$variables$variables, "workflow_variables")
  expect_identical(wf$pre$actions$variables$variables$outcomes, quo(y))
  expect_identical(
    wf$pre$actions$variables$variables$predictors,
    quo(c(x1, x2))
  )
})

test_that("cannot add variables if a recipe already exists", {
  rec <- recipes::recipe(mpg ~ cyl, mtcars)

  wf <- workflow()
  wf <- add_recipe(wf, rec)

  expect_snapshot(error = TRUE, add_variables(wf, y, x))
})

test_that("cannot add variables if a formula already exist", {
  wf <- workflow()
  wf <- add_formula(wf, mpg ~ cyl)

  expect_snapshot(error = TRUE, add_variables(wf, y, x))
})

test_that("informative error if either `predictors` or `outcomes` aren't provided (#144)", {
  expect_snapshot(error = TRUE, add_variables(workflow(), outcomes = mpg))
  expect_snapshot(error = TRUE, add_variables(workflow(), predictors = mpg))
})

test_that("works with fit()", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_variables(workflow, mpg, c(cyl, disp))
  workflow <- add_model(workflow, mod)

  result <- fit(workflow, mtcars)

  expect_identical(
    names(result$fit$fit$fit$coefficients),
    c("(Intercept)", "cyl", "disp")
  )

  expect_identical(
    names(result$pre$mold$outcomes),
    "mpg"
  )

  expect_identical(
    names(result$pre$mold$predictors),
    c("cyl", "disp")
  )
})

test_that("works with fit() when using `variables` specification", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  variables <- workflow_variables(mpg, c(cyl, disp))

  workflow <- workflow()
  workflow <- add_variables(workflow, variables = variables)
  workflow <- add_model(workflow, mod)

  result <- fit(workflow, mtcars)

  expect_identical(
    names(result$fit$fit$fit$coefficients),
    c("(Intercept)", "cyl", "disp")
  )

  expect_identical(
    names(result$pre$mold$outcomes),
    "mpg"
  )

  expect_identical(
    names(result$pre$mold$predictors),
    c("cyl", "disp")
  )
})

test_that("can use a `NULL` outcome", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_variables(workflow, NULL, c(cyl, disp))
  workflow <- add_model(workflow, mod)

  workflow <- .fit_pre(workflow, mtcars)

  expect_named(
    workflow$pre$mold$predictors,
    c("cyl", "disp")
  )

  expect_s3_class(
    workflow$pre$mold$outcomes,
    "tbl_df"
  )

  expect_identical(
    nrow(workflow$pre$mold$outcomes),
    32L
  )

  expect_identical(
    ncol(workflow$pre$mold$outcomes),
    0L
  )
})

test_that("can use `all_of(x)` when `x` is in the scope of `fit()`", {
  x <- c("disp", "cyl")

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_variables(workflow, mpg, all_of(x))
  workflow <- add_model(workflow, mod)

  result <- fit(workflow, mtcars)

  expect_named(
    result$pre$mold$predictors,
    c("disp", "cyl")
  )
})

test_that("`outcomes` are removed from set of possible `predictors` (#72)", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_model(workflow, mod)

  workflow1 <- add_variables(workflow, mpg, everything())

  result <- .fit_pre(workflow1, mtcars)

  mtcars2 <- mtcars
  mtcars2$mpg <- NULL

  expect_identical(
    colnames(result$pre$mold$predictors),
    colnames(mtcars2)
  )

  expect_identical(
    colnames(result$pre$mold$outcomes),
    "mpg"
  )

  workflow2 <- add_variables(workflow, mpg, mpg)

  expect_error(.fit_pre(workflow2, mtcars), class = "vctrs_error_subscript_oob")
})

test_that("selecting no `outcomes` doesn't break selection of `predictors`", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_variables(workflow, any_of("not_here"), everything())
  workflow <- add_model(workflow, mod)

  result <- .fit_pre(workflow, mtcars)

  expect_identical(
    colnames(result$pre$mold$predictors),
    colnames(mtcars)
  )

  expect_identical(
    colnames(result$pre$mold$outcomes),
    character()
  )

  expect_identical(
    nrow(result$pre$mold$outcomes),
    32L
  )
})

test_that("cannot add two variables", {
  workflow <- workflow()
  workflow <- add_variables(workflow, mpg, cyl)

  expect_snapshot(error = TRUE, add_variables(workflow, mpg, cyl))
  expect_snapshot(
    error = TRUE,
    add_variables(workflow, variables = workflow_variables(mpg, cyl))
  )
})

test_that("can remove variables", {
  wf <- workflow()
  wf1 <- add_variables(wf, mpg, cyl)
  wf2 <- remove_variables(wf1)

  expect_equal(wf2$pre, wf$pre)
})

test_that("can remove variables after model fit", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  wf <- workflow()
  wf <- add_model(wf, lm_model)

  wf1 <- add_variables(wf, mpg, cyl)
  wf1 <- fit(wf1, data = mtcars)

  wf2 <- remove_variables(wf1)

  expect_equal(wf$pre, wf2$pre)
  expect_equal(wf$fit, wf2$fit)
})

test_that("removing variables doesn't remove case weights", {
  wf <- workflow()
  wf <- add_variables(wf, mpg, cyl)
  wf <- add_case_weights(wf, disp)

  wf <- remove_variables(wf)

  expect_identical(names(wf$pre$actions), "case_weights")
})

test_that("can update a formula", {
  wf <- workflow()
  wf1 <- add_variables(wf, mpg, cyl)
  wf2 <- update_variables(wf1, cyl, mpg)
  wf3 <- update_variables(wf1, variables = workflow_variables(cyl, mpg))

  expect_identical(
    extract_preprocessor(wf2),
    new_workflow_variables(outcomes = quo(cyl), predictors = quo(mpg))
  )
  expect_identical(
    extract_preprocessor(wf3),
    new_workflow_variables(outcomes = quo(cyl), predictors = quo(mpg))
  )
})

test_that("can only use a 'xy_blueprint' blueprint", {
  blueprint <- hardhat::default_recipe_blueprint()

  workflow <- workflow()

  expect_snapshot(
    error = TRUE,
    add_variables(workflow, mpg, cyl, blueprint = blueprint)
  )
})

test_that("can pass a blueprint through to hardhat::mold()", {
  lm_model <- parsnip::linear_reg()
  lm_model <- parsnip::set_engine(lm_model, "lm")

  blueprint <- hardhat::default_xy_blueprint(intercept = TRUE)

  workflow <- workflow()
  workflow <- add_model(workflow, lm_model)
  workflow <- add_variables(workflow, mpg, cyl, blueprint = blueprint)

  workflow <- fit(workflow, data = mtcars)

  expect_true("(Intercept)" %in% colnames(workflow$pre$mold$predictors))
  expect_identical(workflow$pre$actions$variables$blueprint, blueprint)
  expect_true(workflow$pre$mold$blueprint$intercept)
})
