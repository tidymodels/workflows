skip_if_not_installed("recipes")

test_that("can compute required packages of a workflow - formula", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  expect_true("stats" %in% generics::required_pkgs(workflow))
})

test_that("can compute required packages of a workflow - recipes", {
  skip_if_not_installed("recipes")

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  step <- recipes::step(
    "workflows_test",
    trained = FALSE,
    id = "",
    skip = FALSE,
    role = NA
  )

  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec <- recipes::add_step(rec, step)

  required_pkgs_step_workflows_test <- function(x, ...) {
    "pkg"
  }
  vctrs::s3_register(
    "generics::required_pkgs",
    "step_workflows_test",
    required_pkgs_step_workflows_test
  )

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  expect_true("pkg" %in% generics::required_pkgs(workflow))
})

# ------------------------------------------------------------------------------
# tunable()

test_that("workflow with no tunable parameters", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  data("Chicago")

  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  lm_model <- parsnip::linear_reg() |> parsnip::set_engine("lm")
  wflow_untunable <- workflow(rm_rec, lm_model)

  wflow_info <- tunable(wflow_untunable)
  check_tunable(wflow_info)
  expect_equal(nrow(wflow_info), 0)
})

test_that("extract tuning from workflow with tunable recipe", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  data("Chicago")

  spline_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_date(date) |>
    recipes::step_holiday(date) |>
    recipes::step_rm(date, ends_with("away")) |>
    recipes::step_impute_knn(
      recipes::all_predictors(),
      neighbors = hardhat::tune("imputation")
    ) |>
    recipes::step_other(recipes::all_nominal(), threshold = hardhat::tune()) |>
    recipes::step_dummy(recipes::all_nominal()) |>
    recipes::step_normalize(recipes::all_predictors()) |>
    recipes::step_bs(
      recipes::all_predictors(),
      deg_free = hardhat::tune(),
      degree = hardhat::tune()
    )
  lm_model <- parsnip::linear_reg() |>
    parsnip::set_engine("lm")
  wflow_tunable_recipe <- workflow(spline_rec, lm_model)

  wflow_info <- tunable(wflow_tunable_recipe)
  check_tunable(wflow_info)
  expect_true(all(wflow_info$source == "recipe"))
})

test_that("extract tuning from workflow with tunable model", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  data("Chicago")

  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wflow_tunable_model <- workflow(rm_rec, bst_model)

  wflow_info <- tunable(wflow_tunable_model)
  check_tunable(wflow_info)
  expect_true(all(wflow_info$source == "model_spec"))
})

test_that("extract tuning from workflow with tunable postprocessor", {
  skip_if_not_installed("tailor")

  wflow <- workflow()
  wflow <- add_recipe(wflow, recipes::recipe(mpg ~ ., mtcars))
  wflow <- add_model(wflow, parsnip::linear_reg())
  wflow <- add_tailor(
    wflow,
    tailor::tailor() |>
      tailor::adjust_numeric_range(lower_limit = hardhat::tune())
  )

  wflow_info <- tunable(wflow)

  check_tunable(wflow_info)
  expect_true(all(wflow_info$source == "tailor"))
})

test_that("extract tuning from workflow with tunable recipe and model", {
  skip_if_not_installed("modeldata")
  library(modeldata)
  data("Chicago")

  spline_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_date(date) |>
    recipes::step_holiday(date) |>
    recipes::step_rm(date, ends_with("away")) |>
    recipes::step_impute_knn(
      recipes::all_predictors(),
      neighbors = hardhat::tune("imputation")
    ) |>
    recipes::step_other(recipes::all_nominal(), threshold = hardhat::tune()) |>
    recipes::step_dummy(recipes::all_nominal()) |>
    recipes::step_normalize(recipes::all_predictors()) |>
    recipes::step_bs(
      recipes::all_predictors(),
      deg_free = hardhat::tune(),
      degree = hardhat::tune()
    )
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wflow_tunable <- workflow(spline_rec, bst_model)

  wflow_info <- tunable(wflow_tunable)
  check_tunable(wflow_info)
  expect_equal(
    sort(unique(wflow_info$source)),
    c("model_spec", "recipe")
  )
})

test_that("extract tuning from workflow with tunable recipe, model, and tailor", {
  skip_if_not_installed("tailor")

  wflow <- workflow()
  wflow <- add_recipe(
    wflow,
    recipes::recipe(mpg ~ ., mtcars) |>
      recipes::step_impute_knn(
        recipes::all_predictors(),
        neighbors = hardhat::tune("imputation")
      )
  )
  wflow <- add_model(
    wflow,
    parsnip::linear_reg(engine = "glmnet", penalty = tune())
  )
  wflow <- add_tailor(
    wflow,
    tailor::tailor() |>
      tailor::adjust_numeric_range(lower_limit = hardhat::tune())
  )

  wflow_info <- tunable(wflow)

  check_tunable(wflow_info)
  expect_equal(
    sort(unique(wflow_info$source)),
    c("model_spec", "recipe", "tailor")
  )
})
