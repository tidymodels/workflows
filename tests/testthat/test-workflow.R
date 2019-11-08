test_that("can create a basic workflow", {
  expect_is(workflow(), "workflow")
  expect_is(workflow()$pre, "stage_pre")
  expect_is(workflow()$fit, "stage_fit")
  expect_is(workflow()$post, "stage_post")
})
