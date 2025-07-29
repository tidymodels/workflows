#' Add case weights to a workflow
#'
#' @description
#' This family of functions revolves around selecting a column of `data` to use
#' for _case weights_. This column must be one of the allowed case weight types,
#' such as [hardhat::frequency_weights()] or [hardhat::importance_weights()].
#' Specifically, it must return `TRUE` from [hardhat::is_case_weights()]. The
#' underlying model will decide whether or not the type of case weights you have
#' supplied are applicable or not.
#'
#' - `add_case_weights()` specifies the column that will be interpreted as
#'   case weights in the model. This column must be present in the `data`
#'   supplied to [fit()][fit.workflow()].
#'
#' - `remove_case_weights()` removes the case weights. Additionally, if the
#'   model has already been fit, then the fit is removed.
#'
#' - `update_case_weights()` first removes the case weights, then replaces them
#'   with the new ones.
#'
#' @details
#' For formula and variable preprocessors, the case weights `col` is removed
#' from the data before the preprocessor is evaluated. This allows you to use
#' formulas like `y ~ .` or tidyselection like `everything()` without fear of
#' accidentally selecting the case weights column.
#'
#' For recipe preprocessors, the case weights `col` is not removed and is
#' passed along to the recipe. Typically, your recipe will include steps that
#' can utilize case weights.
#'
#' @param x A workflow
#'
#' @param col A single unquoted column name specifying the case weights for
#'   the model. This must be a classed case weights column, as determined by
#'   [hardhat::is_case_weights()].
#'
#' @export
#' @examples
#' library(parsnip)
#' library(magrittr)
#' library(hardhat)
#'
#' mtcars2 <- mtcars
#' mtcars2$gear <- frequency_weights(mtcars2$gear)
#'
#' spec <- linear_reg() |>
#'   set_engine("lm")
#'
#' wf <- workflow() |>
#'   add_case_weights(gear) |>
#'   add_formula(mpg ~ .) |>
#'   add_model(spec)
#'
#' wf <- fit(wf, mtcars2)
#'
#' # Notice that the case weights (gear) aren't included in the predictors
#' extract_mold(wf)$predictors
#'
#' # Strip them out of the workflow, which also resets the model
#' remove_case_weights(wf)
add_case_weights <- function(x, col) {
  col <- enquo(col)
  action <- new_action_case_weights(col)
  # Ensures that case-weight actions are always before preprocessor actions
  add_action(x, action, "case_weights")
}

#' @rdname add_case_weights
#' @export
remove_case_weights <- function(x) {
  validate_is_workflow(x)

  if (!has_case_weights(x)) {
    cli_warn("The workflow has no case weights specification to remove.")
  }

  actions <- x$pre$actions
  actions[["case_weights"]] <- NULL

  new_workflow(
    pre = new_stage_pre(actions = actions),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_case_weights
#' @export
update_case_weights <- function(x, col) {
  x <- remove_case_weights(x)
  add_case_weights(x, {{ col }})
}

# ------------------------------------------------------------------------------

#' @export
fit.action_case_weights <- function(object, workflow, data, ...) {
  col <- object$col

  loc <- eval_select_case_weights(col, data)

  case_weights <- data[[loc]]

  if (!hardhat::is_case_weights(case_weights)) {
    cli_abort(c(
      "{.arg col} must select a classed case weights column, as determined by
       {.fun hardhat::is_case_weights}.",
      "i" = "For example, it could be a column created by
             {.fun hardhat::frequency_weights} or
             {.fun hardhat::importance_weights}."
    ))
  }

  # Remove case weights for formula/variable preprocessors so `y ~ .` and
  # `everything()` don't pick up the weights column. Recipe preprocessors
  # likely need the case weights columns so we don't remove them in that case.
  # They will be automatically tagged by the recipe with a `"case_weights"`
  # role, so they won't be considered predictors during `bake()`, meaning
  # that passing them through should be harmless.
  remove <-
    has_preprocessor_formula(workflow) ||
    has_preprocessor_variables(workflow)

  if (remove) {
    data[[loc]] <- NULL
  }

  workflow$pre <- new_stage_pre(
    actions = workflow$pre$actions,
    mold = NULL,
    case_weights = case_weights
  )

  # All pre steps return the `workflow` and `data`
  list(workflow = workflow, data = data)
}

# ------------------------------------------------------------------------------

new_action_case_weights <- function(col) {
  if (!is_quosure(col)) {
    cli_abort("{.arg col} must be a quosure.", .internal = TRUE)
  }

  new_action_pre(
    col = col,
    subclass = "action_case_weights"
  )
}

# ------------------------------------------------------------------------------

extract_case_weights_col <- function(x) {
  x$pre$actions$case_weights$col
}

eval_select_case_weights <- function(col, data, ..., call = caller_env()) {
  check_dots_empty()

  # `col` is saved as a quosure, so it carries along the evaluation environment
  env <- empty_env()

  loc <- tidyselect::eval_select(
    expr = col,
    data = data,
    env = env,
    error_call = call
  )

  if (length(loc) != 1L) {
    message <- paste0(
      "{.arg col} must specify exactly one column from
       {.arg data} to extract case weights from."
    )

    cli_abort(message, call = call)
  }

  loc
}
