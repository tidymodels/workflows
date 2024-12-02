#' @export
required_pkgs.workflow <- function(x, infra = TRUE, ...) {
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
      cli_abort(
        "The {.pkg recipes} package must be installed to compute the
         {.fun required_pkgs} of a workflow with a recipe preprocessor."
      )
    }

    pkgs <- generics::required_pkgs(preprocessor, infra = infra)
    out <- c(pkgs, out)
  }

  out <- unique(out)
  out
}

#' @export
tune_args.workflow <- function(object, ...) {
  model <- extract_spec_parsnip(object)

  param_data <- generics::tune_args(model)

  if (has_preprocessor_recipe(object)) {
    recipe <- extract_preprocessor(object)
    recipe_param_data <- generics::tune_args(recipe)
    param_data <- vctrs::vec_rbind(param_data, recipe_param_data)
  }

  if (has_postprocessor_tailor(object)) {
    tailor <- extract_postprocessor(object)
    tailor_param_data <- generics::tune_args(tailor)
    param_data <- vctrs::vec_rbind(param_data, tailor_param_data)
  }

  param_data
}

#' @export
tunable.workflow <- function(x, ...) {
  model <- extract_spec_parsnip(x)
  param_data <- generics::tunable(model)

  if (has_preprocessor_recipe(x)) {
    recipe <- extract_preprocessor(x)
    recipe_param_data <- generics::tunable(recipe)

    param_data <- vctrs::vec_rbind(param_data, recipe_param_data)
  }

  if (has_postprocessor_tailor(x)) {
    tailor <- extract_postprocessor(x)
    tailor_param_data <- generics::tunable(tailor)

    param_data <- vctrs::vec_rbind(param_data, tailor_param_data)
  }

  param_data
}
