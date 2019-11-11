test_that("can create a basic workflow control object", {
  expect_is(control_workflow(), "control_workflow")
})

test_that("default parsnip control is created", {
  expect_equal(control_workflow()$control_parsnip, parsnip::control_parsnip())
})

test_that("parsnip control is validated", {
  expect_error(
    control_workflow(control_parsnip = 1),
    "must be a 'control_parsnip' object"
  )
})
