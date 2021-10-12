test_that("can compute required packages of a workflow - formula", {
  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  expect_true("stats" %in% generics::required_pkgs(workflow))
})

test_that("can compute required packages of a workflow - recipes", {
  skip_if_not_installed("recipes")

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  step <- recipes::step("workflows_test")

  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec <- recipes::add_step(rec, step)

  required_pkgs_step_workflows_test <- function(x, ...) {
    "pkg"
  }
  vctrs::s3_register(
    "generics::required_pkgs",
    "step_workflows_test",
    required_pkgs_step_workflows_test
  )

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  expect_true("pkg" %in% generics::required_pkgs(workflow))
})
