# nocov start

.onLoad <- function(libname, pkgname) {
  ns <- rlang::ns_env("workflows")

  vctrs::s3_register("butcher::axe_call", "workflow")
  vctrs::s3_register("butcher::axe_ctrl", "workflow")
  vctrs::s3_register("butcher::axe_data", "workflow")
  vctrs::s3_register("butcher::axe_env", "workflow")
  vctrs::s3_register("butcher::axe_fitted", "workflow")
}

dummy_withr <- function() withr::defer

# nocov end
