test_that("get recipe input column names", {
  skip_if_not_installed("modeldata")
  skip_if_not_installed("recipes")

  library(recipes)

  data(cells, package = "modeldata")

  cells <- cells[, 1:10]
  pred_names <- sort(names(cells)[3:10])

  rec_with_id <-
    recipes::recipe(class ~ ., cells) %>%
    update_role(case, new_role = "destination") %>%
    step_rm(angle_ch_1) %>%
    step_pca(all_predictors())

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec_with_id)
  workflow <- add_model(workflow, parsnip::logistic_reg())
  workflow_fit <- fit(workflow, cells)

  expect_snapshot(
    workflows:::.get_input_predictors_workflow(workflow),
    error = TRUE
  )
  expect_equal(
    workflows:::.get_input_predictors_workflow(workflow_fit),
    pred_names
  )
  expect_snapshot(
    workflows:::.get_input_predictors_recipe(rec_with_id),
    error = TRUE
  )

})

test_that("get formula input column names", {
  skip_if_not_installed("modeldata")

  data(Chicago, package = "modeldata")

  Chicago <- Chicago[, c("ridership", "date", "Austin")]
  pred_names <- sort(c("date", "Austin"))

  workflow <- workflow()
  workflow <- add_formula(workflow, ridership ~ .)
  workflow <- add_model(workflow, parsnip::linear_reg())
  workflow_fit <- fit(workflow, Chicago)

  expect_snapshot(
    workflows:::.get_input_predictors_workflow(workflow),
    error = TRUE
  )
  expect_equal(
    workflows:::.get_input_predictors_workflow(workflow_fit),
    pred_names
  )

})


test_that("get predictor input column names", {
  skip_if_not_installed("modeldata")

  data(Chicago, package = "modeldata")

  Chicago <- Chicago[, c("ridership", "date", "Austin")]
  pred_names <- sort(c("date", "Austin"))

  workflow <- workflow()
  workflow <-
    add_variables(workflow,
                  outcomes = c(ridership),
                  predictors = c(tidyselect::everything()))
  workflow <- add_model(workflow, parsnip::linear_reg())
  workflow_fit <- fit(workflow, Chicago)

  expect_snapshot(
    workflows:::.get_input_predictors_workflow(workflow),
    error = TRUE
  )
  expect_equal(
    workflows:::.get_input_predictors_workflow(workflow_fit),
    pred_names
  )

})
