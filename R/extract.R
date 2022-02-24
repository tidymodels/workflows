#' Extract elements of a workflow
#'
#' @description
#' These functions extract various elements from a workflow object. If they do
#' not exist yet, an error is thrown.
#'
#' - `extract_preprocessor()` returns the formula, recipe, or variable
#'   expressions used for preprocessing.
#'
#' - `extract_spec_parsnip()` returns the parsnip model specification.
#'
#' - `extract_fit_parsnip()` returns the parsnip model fit object.
#'
#' - `extract_fit_engine()` returns the engine specific fit embedded within
#'   a parsnip model fit. For example, when using [parsnip::linear_reg()]
#'   with the `"lm"` engine, this returns the underlying `lm` object.
#'
#' - `extract_mold()` returns the preprocessed "mold" object returned
#'   from [hardhat::mold()]. It contains information about the preprocessing,
#'   including either the prepped recipe, the formula terms object, or
#'   variable selectors.
#'
#' - `extract_recipe()` returns the recipe. The `estimated` argument specifies
#'    whether the fitted or original recipe is returned.
#'
#' - `extract_parameter_dials()` returns a single dials parameter object.
#'
#' - `extract_parameter_set_dials()` returns a set of dials parameter objects.
#'
#' @param x A workflow
#'
#' @param estimated A logical for whether the original (unfit) recipe or the
#' fitted recipe should be returned. This argument should be named.
#' @param parameter A single string for the parameter ID.
#' @param ... Not currently used.
#'
#' @details
#' Extracting the underlying engine fit can be helpful for describing the
#' model (via `print()`, `summary()`, `plot()`, etc.) or for variable
#' importance/explainers.
#'
#' However, users should not invoke the `predict()` method on an extracted
#' model. There may be preprocessing operations that `workflows` has executed on
#' the data prior to giving it to the model. Bypassing these can lead to errors
#' or silently generating incorrect predictions.
#'
#' *Good*:
#' ```r
#' workflow_fit %>% predict(new_data)
#' ```
#'
#' *Bad*:
#' ```r
#' workflow_fit %>% extract_fit_engine()  %>% predict(new_data)
#' # or
#' workflow_fit %>% extract_fit_parsnip() %>% predict(new_data)
#' ```
#'
#' @return
#' The extracted value from the object, `x`, as described in the description
#' section.
#'
#' @name extract-workflow
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' model <- linear_reg() %>%
#'   set_engine("lm")
#'
#' recipe <- recipe(mpg ~ cyl + disp, mtcars) %>%
#'   step_log(disp)
#'
#' base_wf <- workflow() %>%
#'   add_model(model)
#'
#' recipe_wf <- add_recipe(base_wf, recipe)
#' formula_wf <- add_formula(base_wf, mpg ~ cyl + log(disp))
#' variable_wf <- add_variables(base_wf, mpg, c(cyl, disp))
#'
#' fit_recipe_wf <- fit(recipe_wf, mtcars)
#' fit_formula_wf <- fit(formula_wf, mtcars)
#'
#' # The preprocessor is a recipe, formula, or a list holding the
#' # tidyselect expressions identifying the outcomes/predictors
#' extract_preprocessor(recipe_wf)
#' extract_preprocessor(formula_wf)
#' extract_preprocessor(variable_wf)
#'
#' # The `spec` is the parsnip spec before it has been fit.
#' # The `fit` is the fitted parsnip model.
#' extract_spec_parsnip(fit_formula_wf)
#' extract_fit_parsnip(fit_formula_wf)
#' extract_fit_engine(fit_formula_wf)
#'
#' # The mold is returned from `hardhat::mold()`, and contains the
#' # predictors, outcomes, and information about the preprocessing
#' # for use on new data at `predict()` time.
#' extract_mold(fit_recipe_wf)
#'
#' # A useful shortcut is to extract the fitted recipe from the workflow
#' extract_recipe(fit_recipe_wf)
#'
#' # That is identical to
#' identical(
#'   extract_mold(fit_recipe_wf)$blueprint$recipe,
#'   extract_recipe(fit_recipe_wf)
#' )
NULL

#' @export
#' @rdname extract-workflow
extract_spec_parsnip.workflow <- function(x, ...) {
  if (has_spec(x)) {
    return(x$fit$actions$model$spec)
  }
  abort("The workflow does not have a model spec.")
}

#' @export
#' @rdname extract-workflow
extract_recipe.workflow <- function(x, ..., estimated = TRUE) {
  check_dots_empty()
  if (!is_bool(estimated)) {
    abort("`estimated` must be a single `TRUE` or `FALSE`.")
  }
  if (!has_preprocessor_recipe(x)) {
    abort("The workflow must have a recipe preprocessor.")
  }

  if (estimated) {
    # Gracefully fails if not yet fitted
    mold <- extract_mold(x)
    res <- mold$blueprint$recipe
  } else {
    res <- x$pre$actions$recipe$recipe
  }
  res
}

#' @export
#' @rdname extract-workflow
extract_fit_parsnip.workflow <- function(x, ...) {
  if (has_fit(x)) {
    return(x$fit$fit)
  }
  abort(c(
    "Can't extract a model fit from an untrained workflow.",
    i = "Do you need to call `fit()`?"
  ))
}

#' @export
#' @rdname extract-workflow
extract_fit_engine.workflow <- function(x, ...) {
  extract_fit_parsnip(x)$fit
}

#' @export
#' @rdname extract-workflow
extract_mold.workflow <- function(x, ...) {
  if (has_mold(x)) {
    return(x$pre$mold)
  }
  abort(c(
    "Can't extract a mold from an untrained workflow.",
    i = "Do you need to call `fit()`?"
  ))
}

#' @export
#' @rdname extract-workflow
extract_preprocessor.workflow <- function(x, ...) {
  if (has_preprocessor_formula(x)) {
    return(x$pre$actions$formula$formula)
  }
  if (has_preprocessor_recipe(x)) {
    return(x$pre$actions$recipe$recipe)
  }
  if (has_preprocessor_variables(x)) {
    return(x$pre$actions$variables$variables)
  }
  abort("The workflow does not have a preprocessor.")
}

#' @export
#' @rdname extract-workflow
extract_parameter_set_dials.workflow <- function(x, ...) {
  model <- extract_spec_parsnip(x)
  param_data <- extract_parameter_set_dials(model)

  if (has_preprocessor_recipe(x)) {
    recipe <- extract_preprocessor(x)
    recipe_param_data <- extract_parameter_set_dials(recipe)

    param_data <- vctrs::vec_rbind(param_data, recipe_param_data)
  }

  dials::parameters_constr(
    param_data$name,
    param_data$id,
    param_data$source,
    param_data$component,
    param_data$component_id,
    param_data$object
  )
}

#' @export
#' @rdname extract-workflow
extract_parameter_dials.workflow <- function(x, parameter, ...) {
  extract_parameter_dials(extract_parameter_set_dials(x), parameter)
}
