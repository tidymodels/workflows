#' Create a workflow
#'
#' A `workflow` is a container object that aggregates information required to
#' fit and predict from a model. This information might be a recipe used in
#' preprocessing, specified through [add_recipe()], or the model specification
#' to fit, specified through [add_model()].
#'
#' @return
#' A new `workflow` object.
#'
#' @includeRmd man/rmd/indicators.Rmd details
#'
#' @examples
#' library(parsnip)
#' library(recipes)
#' library(magrittr)
#' library(modeldata)
#'
#' data("attrition")
#'
#' model <- logistic_reg() %>%
#'   set_engine("glm")
#'
#' base_wf <- workflow() %>%
#'   add_model(model)
#'
#' formula_wf <- base_wf %>%
#'   add_formula(Attrition ~ BusinessTravel + YearsSinceLastPromotion + OverTime)
#'
#' fit(formula_wf, attrition)
#'
#' recipe <- recipe(Attrition ~ ., attrition) %>%
#'   step_dummy(all_nominal(), -Attrition) %>%
#'   step_corr(all_predictors(), threshold = 0.8)
#'
#' recipe_wf <- base_wf %>%
#'   add_recipe(recipe)
#'
#' fit(recipe_wf, attrition)
#'
#' variable_wf <- base_wf %>%
#'   add_variables(
#'     Attrition,
#'     c(BusinessTravel, YearsSinceLastPromotion, OverTime)
#'   )
#'
#' fit(variable_wf, attrition)
#' @export
workflow <- function() {
  new_workflow()
}

# ------------------------------------------------------------------------------

new_workflow <- function(pre = new_stage_pre(),
                         fit = new_stage_fit(),
                         post = new_stage_post(),
                         trained = FALSE) {
  if (!is_stage(pre)) {
    abort("`pre` must be a `stage`.")
  }

  if (!is_stage(fit)) {
    abort("`fit` must be a `stage`.")
  }

  if (!is_stage(post)) {
    abort("`post` must be a `stage`.")
  }

  if (!is_scalar_logical(trained)) {
    abort("`trained` must be a single logical value.")
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

#' @export
print.workflow <- function(x, ...) {
  print_header(x)
  print_preprocessor(x)
  print_model(x)
  # print_postprocessor(x)
  invisible(x)
}

print_header <- function(x) {
  if (x$trained) {
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
    spec <- class(pull_workflow_spec(x))[[1]]
    spec <- glue::glue("{spec}()")
  } else {
    spec <- "None"
  }

  spec_msg <- glue::glue("{spec_msg} {spec}")
  cat_line(spec_msg)

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
  formula <- pull_workflow_preprocessor(x)
  formula <- rlang::expr_text(formula)

  cat_line(formula)

  invisible(x)
}

print_preprocessor_variables <- function(x) {
  variables <- pull_workflow_preprocessor(x)

  outcomes <- quo_get_expr(variables$outcomes)
  predictors <- quo_get_expr(variables$predictors)

  outcomes <- expr_text(outcomes)
  predictors <- expr_text(predictors)

  cat_line("Outcomes: ", outcomes)
  cat_line("Predictors: ", predictors)

  invisible(x)
}

print_preprocessor_recipe <- function(x) {
  recipe <- pull_workflow_preprocessor(x)
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

  # Space between Workflow/Preprocessor section and Model section
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
  spec <- pull_workflow_spec(x)

  print(spec)

  invisible(x)
}

print_fit <- function(x) {
  parsnip_fit <- pull_workflow_fit(x)
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

cat_line <- function(...) {
  cat(paste0(..., collapse = "\n"), "\n", sep = "")
}
