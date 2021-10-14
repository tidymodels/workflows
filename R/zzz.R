# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("butcher::axe_call", "workflow")
  vctrs::s3_register("butcher::axe_ctrl", "workflow")
  vctrs::s3_register("butcher::axe_data", "workflow")
  vctrs::s3_register("butcher::axe_env", "workflow")
  vctrs::s3_register("butcher::axe_fitted", "workflow")

  vctrs::s3_register("generics::required_pkgs", "workflow")

  # - If tune isn't installed, register the method (`packageVersion()` will error here)
  # - If tune >= 0.1.6.9001 is installed, register the method
  should_register_tune_args_method <- tryCatch(
    expr = utils::packageVersion("tune") >= "0.1.6.9001",
    error = function(cnd) TRUE
  )

  if (has_at_least_version("tune", "0.1.6.9001")) {
    # `tune_args.workflow()` moved from tune to workflows
    vctrs::s3_register("generics::tune_args", "workflow", tune_args_workflow)
  }
}

# nocov end
