#' Internal environment functions
#'
#' `.add_env()` and `.remove_env()` are internal workflow functions for
#' adding the caller environment to a workflow. They are only exported for
#' usage by the tuning package, [tune](https://github.com/tidymodels/tune),
#' and the general user should never need to worry about them.
#'
#' For [add_variables()], workflows delays the evaluation of the `outcomes` and
#' `predictors` tidyselect expressions until data is available in `fit()`.
#' Rather than capturing quosures, which could result in large environments
#' being stored in the workflow, `add_variables()` instead captures the
#' expressions. To allow users to still be able to use tidyselect expressions
#' like `all_of(x)`, the expression is eventually evaluated in the caller
#' environment of the call to `fit()`. It is likely the case that `x` will
#' still be in that environment, and this allows workflows to not carry around
#' quosures. To avoid this altogether, expert users can still use `all_of(!!x)`
#' to inline the vector. This is admittedly not a perfect solution, but is
#' the best compromise available.
#'
#' `.add_env()` is used to add the caller environment to the workflow before
#' calling [.fit_pre()] and [.fit_model()]. `.remove_env()` removes the
#' caller environment from the workflow after the model has been fit, to
#' ensure that it is not carried around in the workflow afterwards.
#'
#' @param x A workflow
#'
#' @param env The environment to evaluate tidyselect expressions in
#'
#' @name workflows-internals-env
#' @keywords internal
#' @export
#' @examples
#' x <- c("cyl", "disp")
#'
#' wf <- workflow()
#'
#' # Only the expression `all_of(x)` is captured here
#' wf <- add_variables(wf, mpg, all_of(x))
#'
#' # Add the environment containing `x` before preprocessing
#' wf <- .add_env(wf, environment())
#'
#' # Preprocess the workflow data
#' wf <- .fit_pre(wf, mtcars)
#'
#' # Remove the environment to ensure it isn't carried around
#' wf <- .remove_env(wf)
#'
#' # Check that cyl / disp were found as predictors
#' wf$pre$mold$predictors
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

#' @rdname workflows-internals-env
#' @export
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
