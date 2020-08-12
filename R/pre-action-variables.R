add_variables <- function(x,
                          outcomes,
                          predictors,
                          ...,
                          blueprint = NULL) {
  ellipsis::check_dots_empty()

  # Capture as expression to avoid capturing (possibly large) environments
  outcomes <- enexpr(outcomes)
  predictors <- enexpr(predictors)

  # Squash nested quosures resulting from wrapper functions using `{{ }}`
  # to ensure that we get a bare expression
  outcomes <- tidymodels_quo_squash(outcomes)
  predictors <- tidymodels_quo_squash(predictors)

  action <- new_action_variables(outcomes, predictors, blueprint)

  add_action(x, action, "variables")
}

remove_variables <- function(x) {
  validate_is_workflow(x)

  if (!has_preprocessor_variables(x)) {
    rlang::warn("The workflow has no variables preprocessor to remove.")
  }

  new_workflow(
    pre = new_stage_pre(),
    fit = new_stage_fit(actions = x$fit$actions),
    post = new_stage_post(actions = x$post$actions),
    trained = FALSE,
    env = x$env
  )
}

update_variables <- function(x,
                             outcomes,
                             predictors,
                             ...,
                             blueprint = NULL) {
  ellipsis::check_dots_empty()

  x <- remove_variables(x)

  outcomes <- enexpr(outcomes)
  predictors <- enexpr(predictors)

  add_variables(
    x = x,
    outcomes = !!outcomes,
    predictors = !!predictors,
    blueprint = blueprint
  )
}

# ------------------------------------------------------------------------------

fit.action_variables <- function(object, workflow, data) {
  outcomes <- object$outcomes
  predictors <- object$predictors
  blueprint <- object$blueprint

  # Evaluate all expressions in the caller env of `fit()`.
  # This is the best we can do to still allow `all_of(x)` while not capturing
  # outcomes/predictors as quosures with potentially large environments.
  env <- workflow$env

  outcomes <- tidyselect::eval_select(outcomes, data = data, env = env)
  predictors <- tidyselect::eval_select(predictors, data = data, env = env)

  data_outcomes <- data[outcomes]
  data_predictors <- data[predictors]

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

# ------------------------------------------------------------------------------

# Note:
# `quo_squash()` will flatten all quosures and nested quosures, and will
# return a bare expression. We use this to ensure that wrappers of tidymodels
# pipelines can use `{{ }}` to pass expressions along to tidymodels
# functions with tidyselect semantics without capturing them as quosures
# (which is what `{{ }}` usually does). The alternative is to encourage wrappers
# to use `!!enexpr(x)`, which is "more correct" since it won't capture as a
# quosure, but is not a tidy-eval interface we want to encourage usage of.
# This is not a perfect solution, and `quo_squash()` is potentially dangerous
# to use, but it is justified for how tidymodels uses it to avoid the
# possibility of serializing environments captured in specification objects.
tidymodels_quo_squash <- function(quo) {
  quo_squash(quo, warn = FALSE)
}
