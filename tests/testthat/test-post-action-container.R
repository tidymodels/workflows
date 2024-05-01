test_that("can add a postprocessor to a workflow", {
  post <- container::container("regression", "regression")

  workflow <- workflow()
  workflow <- add_container(workflow, post)

  expect_s3_class(workflow$post$actions$container, "action_container")
})

test_that("postprocessor is validated", {
  expect_snapshot(error = TRUE, add_container(workflow(), 1))
})

test_that("cannot add two postprocessors", {
  post <- container::container("regression", "regression")

  workflow <- workflow()
  workflow <- add_container(workflow, post)

  expect_snapshot(error = TRUE, add_container(workflow, post))
})

test_that("remove a postprocessor", {
  post <- container::container("regression", "regression")

  workflow_no_post <- workflow()
  workflow_no_post <- add_formula(workflow_no_post, mpg ~ cyl)

  workflow_with_post <- add_container(workflow_no_post, post)
  workflow_removed_post <- remove_container(workflow_with_post)

  expect_equal(workflow_no_post$post, workflow_removed_post$post)
})

test_that("remove a postprocessor after postprocessor fit", {
  post <- container::container("regression", "regression")

  workflow_no_post <- workflow()
  workflow_no_post <- add_formula(workflow_no_post, mpg ~ cyl)
  workflow_no_post <- add_model(workflow_no_post, parsnip::linear_reg())

  workflow_with_post <- add_container(workflow_no_post, post)
  workflow_with_post <- fit(workflow_with_post, data = mtcars)

  workflow_removed_post <- remove_container(workflow_with_post)

  expect_equal(workflow_no_post$post, workflow_removed_post$post)
})

test_that("update a postprocessor", {
  post <- container::container("regression", "regression")
  post2 <- container::adjust_numeric_range(post, 0, Inf)

  workflow <- workflow()
  workflow <- add_container(workflow, post)
  workflow <- update_container(workflow, post2)

  expect_length(workflow$post$actions$container$container$operations, 1)
})

test_that("update a postprocessor after postprocessor fit", {
  post <- container::container("regression", "regression")
  post2 <- container::adjust_numeric_range(post, 0, Inf)

  workflow_no_post <- workflow()
  workflow_no_post <- add_formula(workflow_no_post, mpg ~ cyl)
  workflow_no_post <- add_model(workflow_no_post, parsnip::linear_reg())

  workflow_with_post <- add_container(workflow_no_post, post)
  workflow_with_post <- fit(workflow_with_post, data = mtcars)

  workflow_with_post_new <- update_container(workflow_with_post, post2)

  expect_length(workflow_with_post_new$post$actions$container$container$operations, 1)

  # Note that the fitted model and preprocessor can remain; the new
  # postprocessor will not affect it (#225)
  expect_equal(workflow_with_post$fit, workflow_with_post_new$fit)
})
