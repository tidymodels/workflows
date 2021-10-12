# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("butcher::axe_call", "workflow")
  vctrs::s3_register("butcher::axe_ctrl", "workflow")
  vctrs::s3_register("butcher::axe_data", "workflow")
  vctrs::s3_register("butcher::axe_env", "workflow")
  vctrs::s3_register("butcher::axe_fitted", "workflow")

  vctrs::s3_register("generics::required_pkgs", "workflow")
}

# nocov end
