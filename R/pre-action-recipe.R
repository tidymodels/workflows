#' Add a recipe to a workflow
#'
#' @description
#' - `add_recipe()` specifies the terms of the model and any preprocessing that
#'   is required through the usage of a recipe.
#'
#' - `remove_recipe()` removes the recipe as well as any downstream objects
#'   that might get created after the recipe is used for preprocessing, such as
#'   the prepped recipe. Additionally, if the model has already been fit, then
#'   the fit is removed.
#'
#' - `update_recipe()` first removes the recipe, then replaces the previous
#'   recipe with the new one. Any model that has already been fit based on this
#'   recipe will need to be refit.
#'
#' @details
#' To fit a workflow, exactly one of [add_formula()], [add_recipe()], or
#' [add_variables()] _must_ be specified.
#'
#' @param x A workflow
#'
#' @param recipe A recipe created using [recipes::recipe()]. The recipe
#'    should not have been trained already with [recipes::prep()]; workflows
#'    will handle training internally.
#'
#' @param ... Not used.
#'
#' @param blueprint A hardhat blueprint used for fine tuning the preprocessing.
#'
#'   If `NULL`, [hardhat::default_recipe_blueprint()] is used.
#'
#'   Note that preprocessing done here is separate from preprocessing that
#'   might be done automatically by the underlying model.
#'
#' @return
#' `x`, updated with either a new or removed recipe preprocessor.
#'
#' @export
#' @examplesIf rlang::is_installed("recipes")
#' library(recipes)
#' library(magrittr)
#'
#' recipe <- recipe(mpg ~ cyl, mtcars) |>
#'   step_log(cyl)
#'
#' workflow <- workflow() |>
#'   add_recipe(recipe)
#'
#' workflow
#'
#' remove_recipe(workflow)
#'
#' update_recipe(workflow, recipe(mpg ~ cyl, mtcars))
add_recipe <- function(x, recipe, ..., blueprint = NULL) {
  check_dots_empty()
  validate_recipes_available()
  action <- new_action_recipe(recipe, blueprint)
  add_action(x, action, "recipe")
}

#' @rdname add_recipe
#' @export
remove_recipe <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_recipe(x)) {
    cli_warn("The workflow has no recipe preprocessor to remove.")
  }

  actions <- x$pre$actions
  actions[["recipe"]] <- NULL

  new_workflow(
    pre = new_stage_pre(actions = actions),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE
  )
}

#' @rdname add_recipe
#' @export
update_recipe <- function(x, recipe, ..., blueprint = NULL) {
  check_dots_empty()
  x <- remove_recipe(x)
  add_recipe(x, recipe, blueprint = blueprint)
}

# ------------------------------------------------------------------------------

#' @export
fit.action_recipe <- function(object, workflow, data, ...) {
  recipe <- object$recipe
  blueprint <- object$blueprint

  mold <- hardhat::mold(recipe, data, blueprint = blueprint)

  if (has_case_weights(workflow)) {
    workflow <- update_retained_case_weights(workflow, mold)
  }

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
check_conflicts.action_recipe <- function(action, x, ..., call = caller_env()) {
  pre <- x$pre

  if (has_action(pre, "formula")) {
    cli_abort(
      "A recipe cannot be added when a formula already exists.",
      call = call
    )
  }
  if (has_action(pre, "variables")) {
    cli_abort(
      "A recipe cannot be added when variables already exist.",
      call = call
    )
  }

  invisible(action)
}

# ------------------------------------------------------------------------------

new_action_recipe <- function(recipe, blueprint, ..., call = caller_env()) {
  check_dots_empty()

  if (!is_recipe(recipe)) {
    cli_abort("{.arg recipe} must be a recipe.", call = call)
  }

  if (recipes::fully_trained(recipe)) {
    cli_abort("Can't add a trained recipe to a workflow.", call = call)
  }

  # `NULL` blueprints are finalized at fit time
  if (!is_null(blueprint) && !is_recipe_blueprint(blueprint)) {
    cli_abort(
      "{.arg blueprint} must be a hardhat {.cls recipe_blueprint}.",
      call = call
    )
  }

  new_action_pre(
    recipe = recipe,
    blueprint = blueprint,
    subclass = "action_recipe"
  )
}

is_recipe <- function(x) {
  inherits(x, "recipe")
}

is_recipe_blueprint <- function(x) {
  inherits(x, "recipe_blueprint")
}

update_retained_case_weights <- function(
  workflow,
  mold,
  ...,
  call = caller_env()
) {
  # If the workflow was using case weights, then we retained these case weights
  # in the `$pre$case_weights` slot. However, when a recipe is used we also
  # pass the case weights on to the recipe. It is possible for the recipe to
  # change the number of rows in the data (with a filter or upsample, for
  # example), in which case we need to update the case weights column that we
  # retain in the workflow. We also do quite a few checks to ensure that the
  # recipe doesn't modify or rename the case weights column in any other way.

  col <- extract_case_weights_col(workflow)

  if (!is_quosure(col)) {
    cli_abort(
      "{.arg col} must be a quosure selecting the case weights column.",
      .internal = TRUE,
      call = call
    )
  }

  case_weights_roles <- mold$extras$roles$case_weights

  if (!is.data.frame(case_weights_roles)) {
    message <- c(
      "No columns with a {.val case_weights} role exist in the data after processing the recipe.",
      "i" = "Did you remove or modify the case weights while processing the recipe?"
    )
    cli_abort(message, call = call)
  }

  loc <- eval_select_case_weights(col, case_weights_roles, call = call)

  case_weights <- case_weights_roles[[loc]]

  if (!hardhat::is_case_weights(case_weights)) {
    message <- c(
      paste0(
        "The column with a recipes role of {.val case_weights} must be a
         classed case weights column, as determined by
         {.fun hardhat::is_case_weights}."
      ),
      "i" = "Did you modify the case weights while processing the recipe?"
    )
    cli_abort(message, call = call)
  }

  workflow$pre$case_weights <- case_weights

  workflow
}
