#' Determine required packages for a workflow
#'
#' @param x A `workflow` object.
#' @param infra Should the core packages themselves be included in the result?
#' Those are workflows and parsnip (for the model), as well as recipes if the
#' workflow includes a recipes preprocessor and tailor if it includes a tailor
#' post-processor.
#' @return A character vector.
#' @keywords internal
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

  if (has_postprocessor_tailor(x)) {
    postprocessor <- extract_postprocessor(x)

    # This also has the side effect of loading tailor, ensuring that its
    # S3 methods for `required_pkgs()` are registered
    if (!is_installed("tailor")) {
      cli_abort(
        "The {.pkg tailor} package must be installed to compute the
         {.fun required_pkgs} of a workflow with a tailor post-processor."
      )
    }

    pkgs <- generics::required_pkgs(postprocessor, infra = infra)
    out <- c(out, pkgs)
  }

  if (infra) {
    out <- c(out, "workflows")
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

  if (has_postprocessor(object)) {
    post <- extract_postprocessor(object)
    post_param_data <- generics::tune_args(post)
    param_data <- vctrs::vec_rbind(param_data, post_param_data)
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

  if (has_postprocessor(x)) {
    post <- extract_postprocessor(x)
    post_param_data <- generics::tunable(post)

    param_data <- vctrs::vec_rbind(param_data, post_param_data)
  }

  param_data
}
