test_that("can create a basic workflow", {
  workflow <- workflow()

  expect_is(workflow, "workflow")

  expect_is(workflow$pre, "stage_pre")
  expect_is(workflow$fit, "stage_fit")
  expect_is(workflow$post, "stage_post")

  expect_equal(workflow$pre$actions, list())
  expect_equal(workflow$pre$mold, NULL)

  expect_equal(workflow$fit$actions, list())
  expect_equal(workflow$fit$fit, NULL)

  expect_equal(workflow$post$actions, list())
})
