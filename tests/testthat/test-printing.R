test_that("can print empty workflow", {
  verify_output(
    test_path("out/test-print-workflow-empty.txt"),
    workflow()
  )
})

test_that("can print workflow with recipe", {
  rec <- recipes::recipe(mtcars)

  verify_output(
    test_path("out/test-print-workflow-recipe.txt"),
    add_recipe(workflow(), rec)
  )
})

test_that("can print workflow with formula", {
  verify_output(
    test_path("out/test-print-workflow-formula.txt"),
    add_formula(workflow(), y ~ x)
  )
})

test_that("can print workflow with variables", {
  verify_output(
    test_path("out/test-print-workflow-variables.txt"),
    add_variables(workflow(), y, c(x1, x2))
  )
})

test_that("can print workflow with model", {
  model <- parsnip::linear_reg()

  verify_output(
    test_path("out/test-print-workflow-model.txt"),
    add_model(workflow(), model)
  )
})

test_that("can print workflow with model with engine specific args", {
  model <- parsnip::linear_reg(penalty = 0.01)
  model <- parsnip::set_engine(model, "glmnet", dfmax = 5)

  verify_output(
    test_path("out/test-print-workflow-model-args.txt"),
    add_model(workflow(), model)
  )
})

test_that("can print workflow with fit model", {
  model <- parsnip::linear_reg()
  model <- parsnip::set_engine(model, "lm")

  workflow <- workflow()
  workflow <- add_formula(workflow, mpg ~ cyl)
  workflow <- add_model(workflow, model)

  verify_output(
    test_path("out/test-print-workflow-fit.txt"),
    fit(workflow, mtcars)
  )
})

test_that("can print workflow with >10 recipe steps", {
  rec <- recipes::recipe(mpg~cyl, mtcars)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)
  rec <- recipes::step_log(rec, cyl)

  add_recipe(workflow(), rec)

  verify_output(
    test_path("out/test-print-workflow-recipe-11-steps.txt"),
    add_recipe(workflow(), rec)
  )

  rec <- recipes::step_log(rec, cyl)

  verify_output(
    test_path("out/test-print-workflow-recipe-12-steps.txt"),
    add_recipe(workflow(), rec)
  )
})
