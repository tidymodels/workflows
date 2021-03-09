test_that("can compute required packages of a workflow - formula", {
  skip_if_not_installed("tune")

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, mod)

  expect_true("stats" %in% generics::required_pkgs(workflow))
})

test_that("can compute required packages of a workflow - recipes", {
  skip_if_not_installed("tune")
  skip_if_not_installed("recipes")

  mod <- parsnip::linear_reg()
  mod <- parsnip::set_engine(mod, "lm")

  rec <- recipes::recipe(mpg ~ cyl, mtcars)
  rec <- recipes::step_ica(rec, cyl)

  workflow <- workflow()
  workflow <- add_recipe(workflow, rec)
  workflow <- add_model(workflow, mod)

  expect_true("dimRed" %in% generics::required_pkgs(workflow))
})
