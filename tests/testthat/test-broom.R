skip_if_not_installed("recipes")

# ------------------------------------------------------------------------------
# tidy()

test_that("can't tidy the model of an unfit workflow", {
  x <- workflow()
  expect_snapshot(error = TRUE, tidy(x))
})

test_that("can't tidy the recipe of an unfit workflow", {
  x <- workflow()

  expect_snapshot(error = TRUE, tidy(x, what = "recipe"))

  rec <- recipes::recipe(y ~ x, data.frame(y = 1, x = 1))

  x <- add_recipe(x, rec)

  expect_snapshot(error = TRUE, tidy(x, what = "recipe"))
})

test_that("can tidy workflow model or recipe", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 3))

  rec <- recipes::recipe(y ~ x, df)
  rec <- recipes::step_log(rec, x)

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_recipe(wf, rec)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  x <- tidy(wf)
  expect_identical(x$term, c("(Intercept)", "x"))

  x <- tidy(wf, what = "recipe")
  expect_identical(x$number, 1L)
})

# ------------------------------------------------------------------------------
# glance()

test_that("can't glance at the model of an unfit workflow", {
  x <- workflow()
  expect_snapshot_error(glance(x))
})

test_that("can glance at a fitted workflow's model", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 3))

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  x <- glance(wf)

  expect_s3_class(x, "tbl_df")
  expect_identical(nrow(x), 1L)
})

# ------------------------------------------------------------------------------
# augment()

test_that("can't augment with the model of an unfit workflow", {
  x <- workflow()
  x <- add_model(x, parsnip::linear_reg())
  expect_snapshot_error(augment(x, mtcars))
})

test_that("can augment using a fitted workflow's model", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 3))

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  x <- augment(wf, df)

  expect_s3_class(x, "tbl_df")
  expect_identical(nrow(x), 3L)

  # at least 1 prediction specific column should be added
  expect_true(ncol(x) > ncol(df))

  expect_named(x, c(".pred", ".resid", "y", "x"))
})

test_that("can augment with a postprocessor (#275)", {
  skip_if_not_installed("tailor")
  skip_if_not_installed("probably")

  cal_post <-
    tailor::tailor() |>
    tailor::adjust_numeric_calibration(method = "isotonic_boot")

  wflow <- workflow(mpg ~ ., parsnip::linear_reg())
  wflow_post <- workflow(mpg ~ ., parsnip::linear_reg(), cal_post)

  # fit the postprocessor as part of the workflow
  set.seed(1)
  fit_post <- fit(
    wflow_post,
    data = mtcars[1:20, ],
    calibration = mtcars[20:30, ]
  )
  pred_post <- fit_post |> augment(mtcars[31:32, ])

  # manually fit the model and the apply the postprocessor
  set.seed(1)
  fit_model <- fit(wflow, mtcars[1:20, ])
  post_fit <- extract_postprocessor(fit_post)$adjustments[[1]]$results$fit
  pred_post_manual <- probably::cal_apply(
    augment(fit_model, mtcars[31:32, ]),
    post_fit
  )

  expect_equal(pred_post, pred_post_manual, ignore_attr = TRUE)
})

test_that("can augment without outcome column", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 3))
  df_new <- df["x"]

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  x <- augment(wf, df_new)

  expect_s3_class(x, "tbl_df")
  expect_identical(nrow(x), 3L)

  # at least 1 prediction specific column should be added
  expect_true(ncol(x) > ncol(df_new))

  expect_named(x, c(".pred", "x"))
})

test_that("augment returns `new_data`, not the pre-processed version of `new_data`", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = factor(c("a", "b", "a")))

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  # Returns `new_data` + prediction columns
  x <- augment(wf, df)

  expect_true(all(names(df) %in% names(x)))
})

test_that("augment fails if it can't preprocess `new_data`", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = factor(c("a", "b", "a")))
  new_data <- data.frame(y = c(2, 3, 4), x = 1:3)

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  expect_error(augment(wf, new_data), class = "vctrs_error_cast")
})

test_that("augment works with matrix compositions (#148)", {
  skip_if_not_installed("broom")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 2), z = c(6, 8, 10))
  new_data <- data.frame(x = 1:3, z = 4:6)

  bp <- hardhat::default_formula_blueprint(composition = "matrix")

  lm_spec <- parsnip::linear_reg()
  lm_spec <- parsnip::set_engine(lm_spec, "lm")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x + z, blueprint = bp)
  wf <- add_model(wf, lm_spec)

  wf <- fit(wf, df)

  out <- augment(wf, new_data = new_data)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c(".pred", "x", "z"))
})

test_that("augment works with sparse matrix compositions (#148)", {
  skip_if_not_installed("broom")

  # These two dependencies aren't in Suggests, so mainly we just run this test
  # locally. They are only used for broom tests, and we don't want to bloat
  # Suggests just for broom support.
  skip_if_not_installed("Matrix")
  # A parsnip engine that supports sparse matrices
  skip_if_not_installed("ranger")

  df <- data.frame(y = c(2, 3, 4), x = c(1, 5, 2), z = c(6, 8, 10))
  new_data <- data.frame(x = 1:3, z = 4:6)

  bp <- hardhat::default_formula_blueprint(composition = "dgCMatrix")

  spec <- parsnip::rand_forest(mode = "regression", engine = "ranger")

  wf <- workflow()
  wf <- add_formula(wf, y ~ x + z, blueprint = bp)
  wf <- add_model(wf, spec)

  wf <- fit(wf, df)

  out <- augment(wf, new_data = new_data)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c(".pred", "x", "z"))
})
