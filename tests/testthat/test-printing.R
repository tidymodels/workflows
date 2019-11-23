
library(recipes)
library(parsnip)

# ------------------------------------------------------------------------------

car_rec <-
  recipe(mpg ~ ., data = mtcars) %>%
  step_normalize(all_predictors()) %>%
  step_ns(disp)

lm_mod <- linear_reg() %>% set_engine("lm")

# ------------------------------------------------------------------------------

test_that("printing an empty workflow", {

  expect_output(
    print(workflow()),
    "── model workflow  ───"
  )
  expect_output(
    print(workflow()),
    "no pre-processors"
  )
  expect_output(
    print(workflow()),
    "\nmodel"
  )
  expect_output(
    print(workflow()),
    "no model object"
  )

})

# ------------------------------------------------------------------------------

test_that("printing a workflow containing only a formula", {

  wflow <- workflow() %>% add_formula(mpg ~ .)

  expect_output(
    print(wflow),
    "── model workflow  ───"
  )
  expect_output(
    print(wflow),
    "\nformula"
  )
  expect_output(
    print(wflow),
    "mpg ~ ."
  )
  expect_output(
    print(wflow),
    "\nmodel"
  )
  expect_output(
    print(wflow),
    "no model object"
  )

})


# ------------------------------------------------------------------------------

test_that("printing a workflow containing only a recipe", {

  wflow <- workflow() %>% add_recipe(car_rec)

  expect_output(
    print(wflow),
    "── model workflow  ───"
  )
  expect_output(
    print(wflow),
    "\nrecipe"
  )
  expect_output(
    print(wflow),
    "step_normalize, and step_ns"
  )
  expect_output(
    print(wflow),
    "\nmodel"
  )
  expect_output(
    print(wflow),
    "no model object"
  )

})

# ------------------------------------------------------------------------------

test_that("printing a workflow containing an unfit model", {

  wflow <- workflow() %>% add_recipe(car_rec) %>% add_model(lm_mod)

  expect_output(
    print(wflow),
    "── model workflow  ───"
  )
  expect_output(
    print(wflow),
    "\nrecipe"
  )
  expect_output(
    print(wflow),
    "step_normalize, and step_ns"
  )
  expect_output(
    print(wflow),
    "\nmodel"
  )
  expect_output(
    print(wflow),
    "\nLinear Regression Model Specification \\(regression\\)",
  )
  expect_output(
    print(wflow),
    "\nComputational engine: lm",
  )
})

# ------------------------------------------------------------------------------

test_that("printing a workflow containing a fit model", {

  wflow <- workflow() %>% add_recipe(car_rec) %>% add_model(lm_mod) %>% fit(mtcars)

  expect_output(
    print(wflow),
    "── model workflow  [trained] ──",
    fixed = TRUE
  )
  expect_output(
    print(wflow),
    "\nrecipe"
  )
  expect_output(
    print(wflow),
    "step_normalize, and step_ns"
  )
  expect_output(
    print(wflow),
    "\nmodel"
  )
  expect_output(
    print(wflow),
    "\nstats::lm\\(formula = formula, data = data\\)",
  )
  expect_output(
    print(wflow),
    "24.97234",
  )
})

