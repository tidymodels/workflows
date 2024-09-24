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
