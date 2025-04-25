skip_if_not_installed("recipes")
skip_if_not_installed("modeldata")
skip_if_not_installed("probably")
skip_if_not_installed("tailor")

data(Chicago, package = "modeldata")

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
  expect_snapshot(error = TRUE, extract_preprocessor(workflow()))
})

test_that("error if not a workflow", {
  expect_snapshot(error = TRUE, extract_preprocessor(1))
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
  expect_snapshot(error = TRUE, extract_spec_parsnip(workflow()))
})

test_that("error if not a workflow", {
  expect_snapshot(error = TRUE, extract_spec_parsnip(1))
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
  expect_snapshot(error = TRUE, extract_fit_parsnip(workflow()))
})

test_that("error if not a workflow", {
  expect_snapshot(error = TRUE, extract_fit_parsnip(1))
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
  expect_snapshot(error = TRUE, extract_mold(workflow()))
})

test_that("error if not a workflow", {
  expect_snapshot(error = TRUE, extract_mold(1))
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
  expect_snapshot(
    error = TRUE,
    extract_recipe(workflow, estimated = "yes please")
  )
})

test_that("error if no recipe preprocessor", {
  expect_snapshot(error = TRUE, extract_recipe(workflow()))
})

test_that("error if no mold", {
  recipe <- recipes::recipe(mpg ~ cyl, mtcars)

  workflow <- workflow()
  workflow <- add_recipe(workflow, recipe)

  expect_snapshot(error = TRUE, extract_recipe(workflow))
})

test_that("error if not a workflow", {
  expect_snapshot(error = TRUE, extract_recipe(1))
})

# ------------------------------------------------------------------------------
# extract_parameter_set_dials()

test_that("extract parameter set from workflow with tunable recipe", {
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
  wf_tunable_recipe <- workflow(spline_rec, lm_model)

  wf_info <- extract_parameter_set_dials(wf_tunable_recipe)
  check_parameter_set_tibble(wf_info)
  expect_true(all(wf_info$source == "recipe"))
})

test_that("extract parameter set from workflow with tunable model", {
  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wf_tunable_model <- workflow(rm_rec, bst_model)

  wf_info <- extract_parameter_set_dials(wf_tunable_model)
  check_parameter_set_tibble(wf_info)
  expect_equal(nrow(wf_info), 2)
  expect_true(all(wf_info$source == "model_spec"))
})

test_that("extract parameter set from workflow with tunable postprocessor", {
  wflow <- workflow()
  wflow <- add_recipe(wflow, recipes::recipe(mpg ~ ., mtcars))
  wflow <- add_model(wflow, parsnip::linear_reg())
  wflow <- add_tailor(
    wflow,
    tailor::tailor() |>
      tailor::adjust_numeric_range(lower_limit = hardhat::tune())
  )

  wflow_info <- extract_parameter_set_dials(wflow)

  check_parameter_set_tibble(wflow_info)
  expect_equal(nrow(wflow_info), 1)
  expect_true(all(wflow_info$source == "tailor"))
})

test_that("extract parameter set from workflow with tunable recipe and model", {
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
  wf_tunable <- workflow(spline_rec, bst_model)

  wf_info <- extract_parameter_set_dials(wf_tunable)
  check_parameter_set_tibble(wf_info)
  expect_equal(
    wf_info$source,
    c(rep("model_spec", 2), rep("recipe", 4))
  )
})

test_that("extract parameter set from workflow with tunable recipe, model, and tailor", {
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

  wflow_info <- extract_parameter_set_dials(wflow)

  check_parameter_set_tibble(wflow_info)
  expect_equal(nrow(wflow_info), 3)
  expect_true(all(wflow_info$source %in% c("recipe", "model_spec", "tailor")))
})

test_that("extract parameter set from workflow with potentially conflicting ids (#266)", {
  # re: https://github.com/tidymodels/workflows/pull/266#issuecomment-2417772184
  # specifically concerned that duplicated "threshold" parameters result in
  # an informative error
  wflow <- workflow()
  wflow <- add_recipe(
    wflow,
    recipes::recipe(mpg ~ ., mtcars) |>
      recipes::step_pca(recipes::all_predictors(), threshold = hardhat::tune())
  )
  wflow <- add_model(wflow, parsnip::linear_reg())
  wflow <- add_tailor(
    wflow,
    tailor::tailor() |>
      tailor::adjust_probability_threshold(threshold = hardhat::tune())
  )

  expect_snapshot(
    error = TRUE,
    extract_parameter_set_dials(wflow)
  )

  # ensure that the user can actually do something about it
  wflow <- remove_tailor(wflow)
  wflow <- add_tailor(
    wflow,
    tailor::tailor() |>
      tailor::adjust_probability_threshold(
        threshold = hardhat::tune("unique id")
      )
  )

  wflow_info <- extract_parameter_set_dials(wflow)

  check_parameter_set_tibble(wflow_info)
  expect_equal(nrow(wflow_info), 2)
  expect_true(all(wflow_info$source %in% c("recipe", "tailor")))
})

# ------------------------------------------------------------------------------
# extract_parameter_dials()

test_that("extract single parameter from workflow with tunable recipe", {
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
  wf_tunable_recipe <- workflow(spline_rec, lm_model)

  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "imputation"),
    dials::neighbors()
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "threshold"),
    dials::threshold(c(0, 1 / 10))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "deg_free"),
    dials::spline_degree(range = c(1, 15))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_recipe, "degree"),
    dials::degree_int(c(1, 2))
  )
})

test_that("extract single parameter from workflow with tunable model", {
  rm_rec <- recipes::recipe(ridership ~ ., data = head(Chicago)) |>
    recipes::step_rm(date, ends_with("away"))
  bst_model <-
    parsnip::boost_tree(
      mode = "classification",
      trees = hardhat::tune("funky name \n")
    ) |>
    parsnip::set_engine("C5.0", rules = hardhat::tune(), noGlobalPruning = TRUE)
  wf_tunable_model <- workflow(rm_rec, bst_model)

  expect_equal(
    hardhat::extract_parameter_dials(
      wf_tunable_model,
      parameter = "funky name \n"
    ),
    dials::trees(c(1, 100))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable_model, parameter = "rules"),
    NA
  )
})

test_that("extract single parameter from workflow with tunable recipe and model", {
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
  wf_tunable <- workflow(spline_rec, bst_model)

  expect_equal(
    extract_parameter_dials(wf_tunable, "imputation"),
    dials::neighbors()
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, "threshold"),
    dials::threshold(c(0, 1 / 10))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, "deg_free"),
    dials::spline_degree(range = c(1, 15))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, "degree"),
    dials::degree_int(c(1, 2))
  )
  expect_equal(
    hardhat::extract_parameter_dials(wf_tunable, parameter = "funky name \n"),
    dials::trees(c(1, 100))
  )
  expect_equal(
    extract_parameter_dials(wf_tunable, parameter = "rules"),
    NA
  )
})

# ------------------------------------------------------------------------------
# extract_recipe()

test_that("extract_fit_time() works", {
  skip_if_not_installed("recipes")
  rec_spec <- recipes::recipe(mpg ~ ., data = mtcars) |>
    recipes::step_scale(recipes::all_numeric_predictors(), id = "scale") |>
    recipes::step_center(recipes::all_numeric_predictors(), id = "center")

  lm_spec <- parsnip::linear_reg()

  wf <- workflow(rec_spec, lm_spec) |> fit(mtcars)

  res <- extract_fit_time(wf)

  expect_s3_class(res, "tbl_df")
  expect_identical(names(res), c("stage", "stage_id", "elapsed"))
  expect_identical(res$stage, "workflow")
  expect_identical(res$stage_id, "workflow")
  expect_true(is.double(res$elapsed))
  expect_true(res$elapsed >= 0)

  res <- extract_fit_time(wf, summarize = FALSE)

  expect_s3_class(res, "tbl_df")
  expect_identical(names(res), c("stage", "stage_id", "elapsed"))
  expect_identical(
    res$stage,
    c("preprocess", "preprocess", "preprocess", "preprocess", "model")
  )
  expect_identical(
    res$stage_id,
    c("prep.scale", "bake.scale", "prep.center", "bake.center", "linear_reg")
  )
  expect_true(is.double(res$elapsed))
  expect_true(all(res$elapsed >= 0))
})
