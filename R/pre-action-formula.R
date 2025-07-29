#' Add formula terms to a workflow
#'
#' @description
#' - `add_formula()` specifies the terms of the model through the usage of a
#'   formula.
#'
#' - `remove_formula()` removes the formula as well as any downstream objects
#'   that might get created after the formula is used for preprocessing, such as
#'   terms. Additionally, if the model has already been fit, then the fit is
#'   removed.
#'
#' - `update_formula()` first removes the formula, then replaces the previous
#'   formula with the new one. Any model that has already been fit based on this
#'   formula will need to be refit.
#'
#' @details
#' To fit a workflow, exactly one of [add_formula()], [add_recipe()], or
#' [add_variables()] _must_ be specified.
#'
#' @includeRmd man/rmd/add-formula.Rmd details
#'
#' @param x A workflow
#'
#' @param formula A formula specifying the terms of the model. It is advised to
#'   not do preprocessing in the formula, and instead use a recipe if that is
#'   required.
#'
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'
#'   If `NULL`, [hardhat::default_formula_blueprint()] is used and is passed
#'   arguments that best align with the model present in the workflow.
#'
#'   Note that preprocessing done here is separate from preprocessing that
#'   might be done by the underlying model. For example, if a blueprint with
#'   `indicators = "none"` is specified, no dummy variables will be created by
#'   hardhat, but if the underlying model requires a formula interface that
#'   internally uses [stats::model.matrix()], factors will still be expanded to
#'   dummy variables by the model.
#'
#' @return
#' `x`, updated with either a new or removed formula preprocessor.
#'
#' @export
#' @examples
#' workflow <- workflow()
#' workflow <- add_formula(workflow, mpg ~ cyl)
#' workflow
#'
#' remove_formula(workflow)
#'
#' update_formula(workflow, mpg ~ disp)
add_formula <- function(x, formula, ..., blueprint = NULL) {
  check_dots_empty()
  action <- new_action_formula(formula, blueprint)
  add_action(x, action, "formula")
}

#' @rdname add_formula
#' @export
remove_formula <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_formula(x)) {
    cli_warn("The workflow has no formula preprocessor to remove.")
  }

  actions <- x$pre$actions
  actions[["formula"]] <- NULL

  new_workflow(
    pre = new_stage_pre(actions = actions),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_formula
#' @export
update_formula <- function(x, formula, ..., blueprint = NULL) {
  check_dots_empty()
  x <- remove_formula(x)
  add_formula(x, formula, blueprint = blueprint)
}

# ------------------------------------------------------------------------------

#' @export
fit.action_formula <- function(object, workflow, data, ...) {
  formula <- object$formula
  blueprint <- object$blueprint

  if (sparsevctrs::has_sparse_elements(data)) {
    cli::cli_abort(
      "Sparse data cannot be used with the formula interface. Please use
     {.fn add_recipe} or {.fn add_variables} instead."
    )
  }

  # TODO - Strip out the formula environment at some time?
  mold <- hardhat::mold(formula, data, blueprint = blueprint)

  check_for_offset(mold)

  workflow$pre <- new_stage_pre(
    actions = workflow$pre$actions,
    mold = mold,
    case_weights = workflow$pre$case_weights
  )

  # All pre steps return the `workflow` and `data`
  list(workflow = workflow, data = data)
}

check_for_offset <- function(mold, ..., call = caller_env()) {
  check_dots_empty()

  # `hardhat::mold()` specially detects offsets in the formula preprocessor and
  # places them in an "extras" slot. This is useful for modeling package
  # authors, but we don't want users to provide an offset in the formula
  # supplied to `add_formula()` because "extra" columns aren't passed on to
  # parsnip. They should use a model formula instead (#162).
  offset <- mold$extras$offset

  if (!is.null(offset)) {
    message <- c(
      "Can't use an offset in the formula supplied to {.fun add_formula}.",
      "i" = "Instead, specify offsets through a model formula
             in {.code add_model(formula = )}."
    )

    cli_abort(message, call = call)
  }
}

# ------------------------------------------------------------------------------

#' @export
check_conflicts.action_formula <- function(
  action,
  x,
  ...,
  call = caller_env()
) {
  pre <- x$pre

  if (has_action(pre, "recipe")) {
    cli_abort(
      "A formula cannot be added when a recipe already exists.",
      call = call
    )
  }
  if (has_action(pre, "variables")) {
    cli_abort(
      "A formula cannot be added when variables already exist.",
      call = call
    )
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_formula <- function(formula, blueprint, ..., call = caller_env()) {
  check_dots_empty()

  if (!is_formula(formula)) {
    cli_abort("{.arg formula} must be a formula.", call = call)
  }

  # `NULL` blueprints are finalized at fit time
  if (!is_null(blueprint) && !is_formula_blueprint(blueprint)) {
    cli_abort(
      "{.arg blueprint} must be a hardhat {.cls formula_blueprint}.",
      call = call
    )
  }

  new_action_pre(
    formula = formula,
    blueprint = blueprint,
    subclass = "action_formula"
  )
}

is_formula_blueprint <- function(x) {
  inherits(x, "formula_blueprint")
}
