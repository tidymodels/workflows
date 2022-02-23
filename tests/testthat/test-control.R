test_that("can create a basic workflow control object", {
  expect_s3_class(control_workflow(), "control_workflow")
})

test_that("default parsnip control is created", {
  expect_equal(control_workflow()$control_parsnip, parsnip::control_parsnip())
})

test_that("parsnip control is validated", {
  expect_snapshot(error = TRUE, {
    control_workflow(control_parsnip = 1)
  })
})
