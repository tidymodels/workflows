# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("butcher::axe_call", "workflow")
  vctrs::s3_register("butcher::axe_ctrl", "workflow")
  vctrs::s3_register("butcher::axe_data", "workflow")
  vctrs::s3_register("butcher::axe_env", "workflow")
  vctrs::s3_register("butcher::axe_fitted", "workflow")

  # Can't use `rlang::is_installed()` at all, as that doesn't work when
  # called from `.onLoad()` for some reason. Instead, rely on `packageVersion()`
  # erroring when the package isn't installed.
  has_at_least_version <- function(pkg, version) {
    tryCatch(
      expr = utils::packageVersion(pkg) >= version,
      error = function(cnd) FALSE
    )
  }

  if (has_at_least_version("tune", "0.1.3.9001")) {
    # `required_pkgs.workflow()` moved from tune to workflows
    vctrs::s3_register("generics::required_pkgs", "workflow", required_pkgs_workflow)
  }
}

# nocov end
