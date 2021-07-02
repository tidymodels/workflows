# @export - lazily and conditionally registered in .onLoad()
required_pkgs_workflow <- function(x, infra = TRUE, ...) {
  out <- character()

  if (has_spec(x)) {
    model <- extract_spec_parsnip(x)
    pkgs <- generics::required_pkgs(model, infra = infra)
    out <- c(pkgs, out)
  }

  if (has_preprocessor_recipe(x)) {
    preprocessor <- extract_preprocessor(x)

    # This also has the side effect of loading recipes, ensuring that its
    # S3 methods for `required_pkgs()` are registered
    if (!is_installed("recipes")) {
      glubort(
        "The recipes package must be installed to compute the ",
        "`required_pkgs()` of a workflow with a recipe preprocessor."
      )
    }

    pkgs <- generics::required_pkgs(preprocessor, infra = infra)
    out <- c(pkgs, out)
  }

  out <- unique(out)
  out
}
