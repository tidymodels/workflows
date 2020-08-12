.add_env <- function(x, env) {
  validate_is_workflow(x)

  if (!is_environment(env)) {
    abort("`env` must be an environment.")
  }

  if (has_env(x)) {
    abort("An environment has already been added to this workflow.")
  }

  x$env <- env

  x
}

.remove_env <- function(x) {
  validate_is_workflow(x)

  if (!has_env(x)) {
    warn("The workflow has no environment to remove.")
  }

  new_workflow(
    pre = x$pre,
    fit = x$fit,
    post = x$post,
    trained = x$trained,
    env = NULL
  )
}
