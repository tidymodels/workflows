skip_if_not_installed("recipes")

test_that("sparse tibble can be passed to `fit() - recipe", {
  skip_if_not_installed("glmnet")
  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  rec <- recipes::recipe(avg_price_per_room ~ ., data = hotel_data)

  wf_spec <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  expect_no_error(wf_fit <- fit(wf_spec, hotel_data))
})

test_that("sparse tibble can be passed to `fit() - formula", {
  skip_if_not_installed("glmnet")
  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  wf_spec <- workflow() %>%
    add_formula(avg_price_per_room ~ .) %>%
    add_model(spec)

  expect_snapshot(
    error = TRUE,
    wf_fit <- fit(wf_spec, hotel_data)
  )
})

test_that("sparse tibble can be passed to `fit() - xy", {
  skip_if_not_installed("glmnet")
  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  wf_spec <- workflow() %>%
    add_variables(avg_price_per_room, everything()) %>%
    add_model(spec)

  expect_no_error(wf_fit <- fit(wf_spec, hotel_data))
})

test_that("sparse matrices can be passed to `fit() - recipe", {
  skip_if_not_installed("glmnet")
  # Make materialization of sparse vectors throw a message
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 1)

  hotel_data <- sparse_hotel_rates()

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  rec <- recipes::recipe(avg_price_per_room ~ ., data = hotel_data)

  wf_spec <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  # We expect 1 materialization - the outcome
  expect_snapshot(wf_fit <- fit(wf_spec, hotel_data))
})

test_that("sparse matrices can be passed to `fit() - formula", {
  skip_if_not_installed("glmnet")

  hotel_data <- sparse_hotel_rates()

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  wf_spec <- workflow() %>%
    add_formula(avg_price_per_room ~ .) %>%
    add_model(spec)

  expect_snapshot(
    error = TRUE,
    wf_fit <- fit(wf_spec, hotel_data)
  )
})

test_that("sparse matrices can be passed to `fit() - xy", {
  skip_if_not_installed("glmnet")
  # Make materialization of sparse vectors throw a message
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 1)

  hotel_data <- sparse_hotel_rates()

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  wf_spec <- workflow() %>%
    add_variables(avg_price_per_room, everything()) %>%
    add_model(spec)

  # We expect 1 materialization - the outcome
  expect_snapshot(wf_fit <- fit(wf_spec, hotel_data))
})

test_that("sparse tibble can be passed to `predict()`", {
  skip_if_not_installed("glmnet")
  # Make materialization of sparse vectors throw an error
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 3)

  hotel_data <- sparse_hotel_rates(tibble = TRUE)

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  rec <- recipes::recipe(avg_price_per_room ~ ., data = hotel_data)

  wf_spec <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  wf_fit <- fit(wf_spec, hotel_data)

  expect_no_error(predict(wf_fit, hotel_data))
})

test_that("sparse matrix can be passed to `predict()`", {
  skip_if_not_installed("glmnet")
  # Make materialization of sparse vectors throw a warning
  # https://r-lib.github.io/sparsevctrs/dev/reference/sparsevctrs_options.html
  withr::local_options("sparsevctrs.verbose_materialize" = 2)

  hotel_data <- sparse_hotel_rates()

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  rec <- recipes::recipe(avg_price_per_room ~ ., data = hotel_data)

  wf_spec <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  # We know that this will cause 1 warning due to the outcome
  suppressWarnings(
    wf_fit <- fit(wf_spec, hotel_data)
  )

  expect_no_warning(predict(wf_fit, hotel_data))
})

test_that("fit() errors if sparse matrix has no colnames", {
  skip_if_not_installed("glmnet")

  hotel_data <- sparse_hotel_rates()
  colnames(hotel_data) <- NULL

  spec <- parsnip::linear_reg(penalty = 0) %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("glmnet")

  wf_spec <- workflow() %>%
    add_variables(avg_price_per_room, everything()) %>%
    add_model(spec)

  expect_snapshot(
    error = TRUE,
    fit(wf_spec, hotel_data)
  )
})

test_that("toggle_sparsity changes auto to yes", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  tree_spec <- parsnip::boost_tree("regression", "xgboost")

  rec_spec <- recipes::recipe(Sale_Price ~ ., data = ames) %>%
    recipes::step_dummy(recipes::all_nominal_predictors())

  wf_spec <- workflow(rec_spec, tree_spec)

  res <- toggle_sparsity(wf_spec, ames)

  expect_identical(
    extract_preprocessor(res)$steps[[1]]$sparse,
    "yes"
  )
})

test_that("toggle_sparsity doesn't change no", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  tree_spec <- parsnip::boost_tree("regression", "xgboost")

  rec_spec <- recipes::recipe(Sale_Price ~ ., data = ames) %>%
    recipes::step_dummy(recipes::all_nominal_predictors(), sparse = "no")

  wf_spec <- workflow(rec_spec, tree_spec)

  res <- toggle_sparsity(wf_spec, ames)

  expect_identical(
    extract_preprocessor(res)$steps[[1]]$sparse,
    "no"
  )
})

test_that("toggle_sparsity changes auto to no", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  tree_spec <- parsnip::boost_tree("regression", "xgboost")

  # if we only dummy 1 variable it doesn't make the data sparse enough
  rec_spec <- recipes::recipe(Sale_Price ~ ., data = ames) %>%
    recipes::step_dummy(MS_Zoning)

  wf_spec <- workflow(rec_spec, tree_spec)

  res <- toggle_sparsity(wf_spec, ames)

  expect_identical(
    extract_preprocessor(res)$steps[[1]]$sparse,
    "no"
  )
})

test_that("toggle_sparsity doesn't change yes", {
  skip_if_not_installed("glmnet")
  skip_if_not_installed("modeldata")

  data("ames", package = "modeldata")

  tree_spec <- parsnip::boost_tree("regression", "xgboost")

  # if we only dummy 1 variable it doesn't make the data sparse enough
  rec_spec <- recipes::recipe(Sale_Price ~ ., data = ames) %>%
    recipes::step_dummy(MS_Zoning, sparse = "yes")

  wf_spec <- workflow(rec_spec, tree_spec)

  res <- toggle_sparsity(wf_spec, ames)

  expect_identical(
    extract_preprocessor(res)$steps[[1]]$sparse,
    "yes"
  )
})
