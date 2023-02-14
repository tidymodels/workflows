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

  vctrs::s3_register("generics::required_pkgs", "workflow")

  vctrs::s3_register("generics::tune_args", "workflow", tune_args_workflow)

  vctrs::s3_register("generics::tunable", "workflow", tunable_workflow)
}

# nocov end
