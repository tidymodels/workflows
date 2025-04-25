#' Create a workflow
#'
#' @description
#' A `workflow` is a container object that aggregates information required to
#' fit and predict from a model. This information might be a recipe used in
#' preprocessing, specified through [add_recipe()], or the model specification
#' to fit, specified through [add_model()], or a tailor used in postprocessing,
#' specified through [add_tailor()].
#'
#' The `preprocessor` and `spec` arguments allow you to add components to a
#' workflow quickly, without having to go through the `add_*()` functions, such
#' as [add_recipe()] or [add_model()]. However, if you need to control any of
#' the optional arguments to those functions, such as the `blueprint` or the
#' model `formula`, then you should use the `add_*()` functions directly
#' instead.
#'
#' @param preprocessor An optional preprocessor to add to the workflow. One of:
#'   - A formula, passed on to [add_formula()].
#'   - A recipe, passed on to [add_recipe()].
#'   - A [workflow_variables()] object, passed on to [add_variables()].
#'
#' @param spec An optional parsnip model specification to add to the workflow.
#'   Passed on to [add_model()].
#'
#' @param postprocessor An optional [tailor::tailor()] defining
#'   post-processing steps to add to the workflow. Passed on to
#'   [add_tailor()].
#'
#' @return
#' A new `workflow` object.
#'
#' @includeRmd man/rmd/indicators.Rmd details
#'
#' @examplesIf rlang::is_installed(c("recipes", "modeldata"))
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#' library(modeldata)
#'
#' data("attrition")
#'
#' model <- logistic_reg() |>
#'   set_engine("glm")
#'
#' formula <- Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime
#'
#' wf_formula <- workflow(formula, model)
#'
#' fit(wf_formula, attrition)
#'
#' recipe <- recipe(Attrition ~ ., attrition) |>
#'   step_dummy(all_nominal(), -Attrition) |>
#'   step_corr(all_predictors(), threshold = 0.8)
#'
#' wf_recipe <- workflow(recipe, model)
#'
#' fit(wf_recipe, attrition)
#'
#' variables <- workflow_variables(
#'   Attrition,
#'   c(BusinessTravel, YearsSinceLastPromotion, OverTime)
#' )
#'
#' wf_variables <- workflow(variables, model)
#'
#' fit(wf_variables, attrition)
#' @export
workflow <- function(preprocessor = NULL, spec = NULL, postprocessor = NULL) {
  out <- new_workflow()

  if (!is_null(preprocessor)) {
    out <- add_preprocessor(out, preprocessor)
  }

  if (!is_null(spec)) {
    out <- add_model(out, spec)
  }

  if (!is_null(postprocessor)) {
    out <- add_postprocessor(out, postprocessor)
  }

  out
}

add_preprocessor <- function(x, preprocessor, ..., call = caller_env()) {
  check_dots_empty()

  if (is_formula(preprocessor)) {
    return(add_formula(x, preprocessor))
  }

  if (is_recipe(preprocessor)) {
    return(add_recipe(x, preprocessor))
  }

  if (is_workflow_variables(preprocessor)) {
    return(add_variables(x, variables = preprocessor))
  }

  cli_abort(
    "{.arg preprocessor} must be a formula, recipe, or a set of workflow variables.",
    call = call
  )
}

add_postprocessor <- function(x, postprocessor, ..., call = caller_env()) {
  check_dots_empty()

  if (is_tailor(postprocessor)) {
    return(add_tailor(x, postprocessor))
  }

  cli_abort(
    "{.arg postprocessor} must be a tailor.",
    call = call
  )
}

# ------------------------------------------------------------------------------

new_workflow <- function(
  pre = new_stage_pre(),
  fit = new_stage_fit(),
  post = new_stage_post(),
  trained = FALSE
) {
  if (!is_stage(pre)) {
    cli_abort("{.arg pre} must be a `stage`.")
  }

  if (!is_stage(fit)) {
    cli_abort("{.arg fit} must be a `stage`.")
  }

  if (!is_stage(post)) {
    cli_abort("{.arg post} must be a `stage`.")
  }

  if (!is_scalar_logical(trained)) {
    cli_abort("{.arg trained} must be a single logical value.")
  }

  data <- list(
    pre = pre,
    fit = fit,
    post = post,
    trained = trained
  )

  structure(data, class = "workflow")
}

is_workflow <- function(x) {
  inherits(x, "workflow")
}

# ------------------------------------------------------------------------------

#' Determine if a workflow has been trained
#'
#' @description
#' A trained workflow is one that has gone through [`fit()`][fit.workflow],
#' which preprocesses the underlying data, and fits the parsnip model.
#'
#' @param x A workflow.
#'
#' @return A single logical indicating if the workflow has been trained or not.
#'
#' @export
#' @examplesIf rlang::is_installed("recipes")
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#'
#' rec <- recipe(mpg ~ cyl, mtcars)
#'
#' mod <- linear_reg()
#' mod <- set_engine(mod, "lm")
#'
#' wf <- workflow() |>
#'   add_recipe(rec) |>
#'   add_model(mod)
#'
#' # Before any preprocessing or model fitting has been done
#' is_trained_workflow(wf)
#'
#' wf <- fit(wf, mtcars)
#'
#' # After all preprocessing and model fitting
#' is_trained_workflow(wf)
is_trained_workflow <- function(x) {
  validate_is_workflow(x)
  is_true(get_trained(x))
}

# ------------------------------------------------------------------------------

get_trained <- function(x) {
  x[["trained"]]
}
set_trained <- function(x, value) {
  x[["trained"]] <- value
  x
}

# ------------------------------------------------------------------------------

#' @export
print.workflow <- function(x, ...) {
  print_header(x)
  print_preprocessor(x)
  print_case_weights(x)
  print_model(x)
  print_postprocessor(x)
  invisible(x)
}

print_header <- function(x) {
  if (is_trained_workflow(x)) {
    trained <- " [trained]"
  } else {
    trained <- ""
  }

  header <- glue::glue("Workflow{trained}")
  header <- cli::rule(header, line = 2)

  cat_line(header)

  preprocessor_msg <- cli::style_italic("Preprocessor:")

  if (has_preprocessor_formula(x)) {
    preprocessor <- "Formula"
  } else if (has_preprocessor_recipe(x)) {
    preprocessor <- "Recipe"
  } else if (has_preprocessor_variables(x)) {
    preprocessor <- "Variables"
  } else {
    preprocessor <- "None"
  }

  preprocessor_msg <- glue::glue("{preprocessor_msg} {preprocessor}")
  cat_line(preprocessor_msg)

  spec_msg <- cli::style_italic("Model:")

  if (has_spec(x)) {
    spec <- class(extract_spec_parsnip(x))[[1]]
    spec <- glue::glue("{spec}()")
  } else {
    spec <- "None"
  }

  spec_msg <- glue::glue("{spec_msg} {spec}")
  cat_line(spec_msg)

  if (has_postprocessor(x)) {
    cat_line(glue::glue("{cli::style_italic('Postprocessor:')} tailor"))
  }

  invisible(x)
}

print_case_weights <- function(x) {
  if (!has_case_weights(x)) {
    return(invisible(x))
  }

  # Space between Workflow / Preprocessor section and Case Weights section
  cat_line("")

  header <- cli::rule("Case Weights")
  cat_line(header)

  col <- extract_case_weights_col(x)
  col <- quo_get_expr(col)
  col <- expr_text(col)

  cat_line(col)

  invisible(x)
}

print_preprocessor <- function(x) {
  has_preprocessor_formula <- has_preprocessor_formula(x)
  has_preprocessor_recipe <- has_preprocessor_recipe(x)
  has_preprocessor_variables <- has_preprocessor_variables(x)

  no_preprocessor <-
    !has_preprocessor_formula &&
    !has_preprocessor_recipe &&
    !has_preprocessor_variables

  if (no_preprocessor) {
    return(invisible(x))
  }

  # Space between Workflow section and Preprocessor section
  cat_line("")

  header <- cli::rule("Preprocessor")
  cat_line(header)

  if (has_preprocessor_formula) {
    print_preprocessor_formula(x)
  }

  if (has_preprocessor_recipe) {
    print_preprocessor_recipe(x)
  }

  if (has_preprocessor_variables) {
    print_preprocessor_variables(x)
  }

  invisible(x)
}

print_preprocessor_formula <- function(x) {
  formula <- extract_preprocessor(x)
  formula <- rlang::expr_text(formula)

  cat_line(formula)

  invisible(x)
}

print_preprocessor_variables <- function(x) {
  variables <- extract_preprocessor(x)

  outcomes <- quo_get_expr(variables$outcomes)
  predictors <- quo_get_expr(variables$predictors)

  outcomes <- expr_text(outcomes)
  predictors <- expr_text(predictors)

  cat_line("Outcomes: ", outcomes)
  cat_line("Predictors: ", predictors)

  invisible(x)
}

print_preprocessor_recipe <- function(x) {
  recipe <- extract_preprocessor(x)
  steps <- recipe$steps

  n_steps <- length(steps)

  if (n_steps == 1L) {
    step <- "Step"
  } else {
    step <- "Steps"
  }

  n_steps_msg <- glue::glue("{n_steps} Recipe {step}")
  cat_line(n_steps_msg)

  if (n_steps == 0L) {
    return(invisible(x))
  }

  cat_line("")

  step_names <- map_chr(steps, pull_step_name)

  if (n_steps <= 10L) {
    cli::cat_bullet(step_names)
    return(invisible(x))
  }

  extra_steps <- n_steps - 10L
  step_names <- step_names[1:10]

  if (extra_steps == 1L) {
    step <- "step"
  } else {
    step <- "steps"
  }

  extra_dots <- "..."
  extra_msg <- glue::glue("and {extra_steps} more {step}.")

  step_names <- c(step_names, extra_dots, extra_msg)

  cli::cat_bullet(step_names)
  invisible(x)
}

pull_step_name <- function(x) {
  step <- class(x)[[1]]
  glue::glue("{step}()")
}

print_model <- function(x) {
  has_spec <- has_spec(x)

  if (!has_spec) {
    return(invisible(x))
  }

  has_fit <- has_fit(x)

  # Space between Workflow/Preprocessor/Case Weights section and Model section
  cat_line("")

  header <- cli::rule("Model")
  cat_line(header)

  if (has_fit) {
    print_fit(x)
    return(invisible(x))
  }

  print_spec(x)
  invisible(x)
}

print_spec <- function(x) {
  spec <- extract_spec_parsnip(x)

  print(spec)

  invisible(x)
}

print_fit <- function(x) {
  parsnip_fit <- extract_fit_parsnip(x)
  fit <- parsnip_fit$fit

  output <- utils::capture.output(fit)
  n_output <- length(output)

  if (n_output < 50L) {
    print(fit)
    return(invisible(x))
  }

  n_extra_output <- n_output - 50L
  output <- output[1:50]

  extra_output_msg <- glue::glue("and {n_extra_output} more lines.")

  cat_line(output)
  cat_line("")
  cat_line("...")
  cat_line(extra_output_msg)

  invisible(x)
}

print_postprocessor <- function(x) {
  has_postprocessor <- has_postprocessor(x)

  if (!has_postprocessor) {
    return(invisible(x))
  }

  # Space between Model section and Postprocessor section
  cat_line("")

  header <- cli::rule("Postprocessor")
  cat_line(header)

  if (has_postprocessor_tailor(x)) {
    print_postprocessor_tailor(x)
  }

  invisible(x)
}

print_postprocessor_tailor <- function(x) {
  tailor <- extract_postprocessor(x)

  # TODO: this snap currently includes some NA return values and marks the
  # following output as a message rather than output.
  tailor_print <- utils::capture.output(tailor, type = "message")
  cat_line(tailor_print[3:length(tailor_print)])

  invisible(x)
}

cat_line <- function(...) {
  cat(paste0(..., collapse = "\n"), "\n", sep = "")
}
