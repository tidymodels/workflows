#' Add case weights to a workflow
#'
#' @description
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
#' @inheritParams rlang::args_dots_empty
#'
#' @param x A workflow
#'
#' @param col A single unquoted column name specifying the case weights for
#'   the model.
#'
#' @param remove A single logical specifying whether or not to remove the case
#'   weights column from the `data` supplied to `fit()` before the preprocessor
#'   is applied.
#'
#'   For formula preprocessors, variables preprocessors, and for recipe
#'   preprocessors that don't use case weights, this should always be `TRUE`.
#'   This ensures that formulas such as `y ~ .` won't pick up the case weights
#'   column as a predictor.
#'
#'   For recipe preprocessors that use case weights, if the case weights column
#'   used in the recipe is the exact same column as `col`, then set `remove =
#'   FALSE` to retain the case weights column in `data` and pass it on to the
#'   preprocessor.
#'
#' @export
#' @examples
#' library(parsnip)
#' library(magrittr)
#'
#' spec <- linear_reg() %>%
#'   set_engine("lm")
#'
#' wf <- workflow() %>%
#'   add_case_weights(gear) %>%
#'   add_formula(mpg ~ .) %>%
#'   add_model(spec)
#'
#' wf <- fit(wf, mtcars)
#'
#' # Notice that the case weights (gear) aren't included in the predictors
#' extract_mold(wf)$predictors
#'
#' # Strip them out of the workflow, which also resets the model
#' remove_case_weights(wf)
add_case_weights <- function(x, col, ..., remove = TRUE) {
  check_dots_empty()
  col <- enquo(col)
  action <- new_action_case_weights(col, remove)
  # Ensures that case-weight actions are always before preprocessor actions
  add_action(x, action, "case_weights")
}

#' @rdname add_case_weights
#' @export
remove_case_weights <- function(x) {
  validate_is_workflow(x)

  if (!has_case_weights(x)) {
    rlang::warn("The workflow has no case weights specification to remove.")
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
update_case_weights <- function(x, col, ..., remove = TRUE) {
  check_dots_empty()
  x <- remove_case_weights(x)
  add_case_weights(x, {{ col }}, remove = remove)
}

# ------------------------------------------------------------------------------

fit.action_case_weights <- function(object, workflow, data) {
  col <- object$col
  remove <- object$remove

  # `col` is saved as a quosure, so it carries along the evaluation environment
  env <- empty_env()

  loc <- tidyselect::eval_select(
    expr = col,
    data = data,
    env = env
  )

  if (length(loc) != 1L) {
    abort(paste0(
      "`col` must specify exactly one column from ",
      "`data` to extract case weights from."
    ))
  }

  case_weights <- data[[loc]]

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

new_action_case_weights <- function(col, remove) {
  if (!is_quosure(col)) {
    abort("`col` must be a quosure.")
  }

  if (!is_bool(remove)) {
    abort("`remove` must be a single `TRUE` or `FALSE`.")
  }

  new_action_pre(
    col = col,
    remove = remove,
    subclass = "action_case_weights"
  )
}

# ------------------------------------------------------------------------------

extract_case_weights_col <- function(x) {
  x$pre$actions$case_weights$col
}
