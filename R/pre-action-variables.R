#' Add variables to a workflow
#'
#' @description
#' - `add_variables()` specifies the terms of the model through the usage of
#'   [tidyselect::select_helpers] for the `outcomes` and `predictors`.
#'
#' - `remove_variables()` removes the variables. Additionally, if the model
#'   has already been fit, then the fit is removed.
#'
#' - `update_variables()` first removes the variables, then replaces the
#'   previous variables with the new ones. Any model that has already been
#'   fit based on the original variables will need to be refit.
#'
#' - `workflow_variables()` bundles `outcomes` and `predictors` into a single
#'   variables object, which can be supplied to `add_variables()`.
#'
#' @details
#' To fit a workflow, exactly one of [add_formula()], [add_recipe()], or
#' [add_variables()] _must_ be specified.
#'
#' @param x A workflow
#'
#' @param outcomes,predictors Tidyselect expressions specifying the terms
#'   of the model. `outcomes` is evaluated first, and then all outcome columns
#'   are removed from the data before `predictors` is evaluated.
#'   See [tidyselect::select_helpers] for the full range of possible ways to
#'   specify terms.
#'
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'
#'   If `NULL`, [hardhat::default_xy_blueprint()] is used.
#'
#'   Note that preprocessing done here is separate from preprocessing that
#'   might be done by the underlying model.
#'
#' @param variables An alternative specification of `outcomes` and `predictors`,
#'   useful for supplying variables programmatically.
#'
#'   - If `NULL`, this argument is unused, and `outcomes` and `predictors` are
#'     used to specify the variables.
#'
#'   - Otherwise, this must be the result of calling `workflow_variables()` to
#'     create a standalone variables object. In this case, `outcomes` and
#'     `predictors` are completely ignored.
#'
#' @return
#' - `add_variables()` returns `x` with a new variables preprocessor.
#'
#' - `remove_variables()` returns `x` after resetting any model fit and
#'   removing the variables preprocessor.
#'
#' - `update_variables()` returns `x` after removing the variables preprocessor,
#'   and then re-specifying it with new variables.
#'
#' - `workflow_variables()` returns a 'workflow_variables' object containing
#'   both the `outcomes` and `predictors`.
#'
#' @export
#' @examples
#' library(parsnip)
#'
#' spec_lm <- linear_reg()
#' spec_lm <- set_engine(spec_lm, "lm")
#'
#' workflow <- workflow()
#' workflow <- add_model(workflow, spec_lm)
#'
#' # Add terms with tidyselect expressions.
#' # Outcomes are specified before predictors.
#' workflow1 <- add_variables(
#'   workflow,
#'   outcomes = mpg,
#'   predictors = c(cyl, disp)
#' )
#'
#' workflow1 <- fit(workflow1, mtcars)
#' workflow1
#'
#' # Removing the variables of a fit workflow will also remove the model
#' remove_variables(workflow1)
#'
#' # Variables can also be updated
#' update_variables(workflow1, mpg, starts_with("d"))
#'
#' # The `outcomes` are removed before the `predictors` expression
#' # is evaluated. This allows you to easily specify the predictors
#' # as "everything except the outcomes".
#' workflow2 <- add_variables(workflow, mpg, everything())
#' workflow2 <- fit(workflow2, mtcars)
#' extract_mold(workflow2)$predictors
#'
#' # Variables can also be added from the result of a call to
#' # `workflow_variables()`, which creates a standalone variables object
#' variables <- workflow_variables(mpg, c(cyl, disp))
#' workflow3 <- add_variables(workflow, variables = variables)
#' fit(workflow3, mtcars)
add_variables <- function(
  x,
  outcomes,
  predictors,
  ...,
  blueprint = NULL,
  variables = NULL
) {
  check_dots_empty()

  if (is_null(variables)) {
    variables <- workflow_variables({{ outcomes }}, {{ predictors }})
  }

  if (!is_workflow_variables(variables)) {
    cli_abort(
      "{.arg variables} must be a {.cls workflow_variables} object
       created from {.fun workflow_variables}."
    )
  }

  action <- new_action_variables(variables, blueprint)

  add_action(x, action, "variables")
}

#' @rdname add_variables
#' @export
remove_variables <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_variables(x)) {
    cli_warn("The workflow has no variables preprocessor to remove.")
  }

  actions <- x$pre$actions
  actions[["variables"]] <- NULL

  new_workflow(
    pre = new_stage_pre(actions = actions),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_variables
#' @export
update_variables <- function(
  x,
  outcomes,
  predictors,
  ...,
  blueprint = NULL,
  variables = NULL
) {
  check_dots_empty()

  x <- remove_variables(x)

  if (is_null(variables)) {
    variables <- workflow_variables({{ outcomes }}, {{ predictors }})
  }

  add_variables(
    x = x,
    blueprint = blueprint,
    variables = variables
  )
}

# ------------------------------------------------------------------------------

#' @export
fit.action_variables <- function(object, workflow, data, ...) {
  variables <- object$variables
  outcomes <- variables$outcomes
  predictors <- variables$predictors
  blueprint <- object$blueprint

  # `outcomes` and `predictors` should both be quosures,
  # meaning they carry along their own environments to evaluate in.
  env <- empty_env()

  outcomes <- tidyselect::eval_select(
    expr = outcomes,
    data = data,
    env = env
  )

  # Evaluate `predictors` without access to `outcomes`
  not_outcomes <- vec_index_invert(outcomes)
  data_potential_predictors <- data[not_outcomes]

  predictors <- tidyselect::eval_select(
    expr = predictors,
    data = data_potential_predictors,
    env = env
  )

  data_outcomes <- data[outcomes]
  data_predictors <- data_potential_predictors[predictors]

  mold <- hardhat::mold(
    x = data_predictors,
    y = data_outcomes,
    blueprint = blueprint
  )

  workflow$pre <- new_stage_pre(
    actions = workflow$pre$actions,
    mold = mold,
    case_weights = workflow$pre$case_weights
  )

  # All pre steps return the `workflow` and `data`
  list(workflow = workflow, data = data)
}

# ------------------------------------------------------------------------------

#' @export
check_conflicts.action_variables <- function(
  action,
  x,
  ...,
  call = caller_env()
) {
  pre <- x$pre

  if (has_action(pre, "recipe")) {
    cli_abort(
      "Variables cannot be added when a recipe already exists.",
      call = call
    )
  }
  if (has_action(pre, "formula")) {
    cli_abort(
      "Variables cannot be added when a formula already exists.",
      call = call
    )
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_variables <- function(
  variables,
  blueprint,
  ...,
  call = caller_env()
) {
  check_dots_empty()

  # `NULL` blueprints are finalized at fit time
  if (!is_null(blueprint) && !is_xy_blueprint(blueprint)) {
    cli_abort(
      "{.arg blueprint} must be a hardhat {.cls xy_blueprint}.",
      call = call
    )
  }

  new_action_pre(
    variables = variables,
    blueprint = blueprint,
    subclass = "action_variables"
  )
}

is_xy_blueprint <- function(x) {
  inherits(x, "xy_blueprint")
}

# ------------------------------------------------------------------------------

#' @rdname add_variables
#' @export
workflow_variables <- function(outcomes, predictors) {
  # TODO: Use partial evaluation with `eval_resolve()`
  # to only capture expressions
  # https://github.com/r-lib/tidyselect/issues/207
  new_workflow_variables(
    outcomes = enquo(outcomes),
    predictors = enquo(predictors)
  )
}

new_workflow_variables <- function(
  outcomes,
  predictors,
  ...,
  call = caller_env()
) {
  check_dots_empty()

  if (!is_quosure(outcomes)) {
    cli_abort("{.arg outcomes} must be a quosure.", .internal = TRUE)
  }
  if (!is_quosure(predictors)) {
    cli_abort("{.arg predictors} must be a quosure.", .internal = TRUE)
  }
  if (quo_is_missing(outcomes)) {
    cli_abort("{.arg outcomes} can't be missing.", call = call)
  }
  if (quo_is_missing(predictors)) {
    cli_abort("{.arg predictors} can't be missing.", call = call)
  }

  data <- list(
    outcomes = outcomes,
    predictors = predictors
  )

  structure(data, class = "workflow_variables")
}

is_workflow_variables <- function(x) {
  inherits(x, "workflow_variables")
}
