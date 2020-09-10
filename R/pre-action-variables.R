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
#' @return
#' `x`, updated with either a new or removed variables preprocessor.
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
#' pull_workflow_mold(workflow2)$predictors
add_variables <- function(x,
                          outcomes,
                          predictors,
                          ...,
                          blueprint = NULL) {
  ellipsis::check_dots_empty()

  # TODO: Use partial evaluation with `eval_resolve()`
  # to only capture expressions
  # https://github.com/r-lib/tidyselect/issues/207
  outcomes <- enquo(outcomes)
  predictors <- enquo(predictors)

  action <- new_action_variables(outcomes, predictors, blueprint)

  add_action(x, action, "variables")
}

#' @rdname add_variables
#' @export
remove_variables <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_variables(x)) {
    rlang::warn("The workflow has no variables preprocessor to remove.")
  }

  new_workflow(
    pre = new_stage_pre(),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_variables
#' @export
update_variables <- function(x,
                             outcomes,
                             predictors,
                             ...,
                             blueprint = NULL) {
  ellipsis::check_dots_empty()

  x <- remove_variables(x)

  add_variables(
    x = x,
    outcomes = {{ outcomes }},
    predictors = {{ predictors }},
    blueprint = blueprint
  )
}

# ------------------------------------------------------------------------------

fit.action_variables <- function(object, workflow, data) {
  outcomes <- object$outcomes
  predictors <- object$predictors
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

  workflow$pre$mold <- hardhat::mold(
    x = data_predictors,
    y = data_outcomes,
    blueprint = blueprint
  )

  # All pre steps return the `workflow` and `data`
  list(workflow = workflow, data = data)
}

# ------------------------------------------------------------------------------

check_conflicts.action_variables <- function(action, x) {
  pre <- x$pre

  if (has_action(pre, "recipe")) {
    abort("Variables cannot be added when a recipe already exists.")
  }
  if (has_action(pre, "formula")) {
    abort("Variables cannot be added when a formula already exists.")
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_variables <- function(outcomes, predictors, blueprint) {
  # `NULL` blueprints are finalized at fit time
  if (!is_null(blueprint) && !is_xy_blueprint(blueprint)) {
    abort("`blueprint` must be a hardhat 'xy_blueprint'.")
  }

  new_action_pre(
    outcomes = outcomes,
    predictors = predictors,
    blueprint = blueprint,
    subclass = "action_variables"
  )
}

is_xy_blueprint <- function(x) {
  inherits(x, "xy_blueprint")
}
