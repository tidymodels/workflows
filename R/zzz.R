# nocov start

# Set in `.onLoad()` below
tune_contains_tune_args_method <- NULL
tune_contains_tunable_method <- NULL

.onLoad <- function(libname, pkgname) {
  ns <- rlang::ns_env("workflows")

  vctrs::s3_register("butcher::axe_call", "workflow")
  vctrs::s3_register("butcher::axe_ctrl", "workflow")
  vctrs::s3_register("butcher::axe_data", "workflow")
  vctrs::s3_register("butcher::axe_env", "workflow")
  vctrs::s3_register("butcher::axe_fitted", "workflow")
}

# nocov end
