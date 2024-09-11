test_that("sparse tibble can be passed to `fit() - recipe", {
  skip_if_not_installed("xgboost")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  spec <- parsnip::boost_tree() %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("xgboost")

  rec <- recipes::recipe(avg_price_per_room ~ ., data = hotel_data)

  wf_spec <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
  
  expect_no_error(
    wf_fit <- fit(wf_spec, hotel_data)
  )
})

test_that("sparse tibble can be passed to `fit() - formula", {
  skip_if_not_installed("xgboost")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  spec <- parsnip::boost_tree() %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("xgboost")

  wf_spec <- workflow() %>%
    add_formula(avg_price_per_room ~ .) %>%
    add_model(spec)
  
  expect_no_error({
    Sys.sleep(10)
    wf_fit <- fit(wf_spec, hotel_data)
  }
  )
})

test_that("sparse tibble can be passed to `fit() - xy", {
  skip_if_not_installed("xgboost")

  hotel_data <- sparse_hotel_rates()
  hotel_data <- sparsevctrs::coerce_to_sparse_tibble(hotel_data)

  spec <- parsnip::boost_tree() %>%
    parsnip::set_mode("regression") %>%
    parsnip::set_engine("xgboost")

  wf_spec <- workflow() %>%
    add_variables(avg_price_per_room, everything()) %>%
    add_model(spec)
  
  expect_no_error(
    wf_fit <- fit(wf_spec, hotel_data)
  )
})
