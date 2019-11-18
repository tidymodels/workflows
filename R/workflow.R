#' Create a workflow
#'
#' A `workflow` is a container object that aggregates information required to
#' fit and predict from a model. This information might be a recipe used in
#' preprocessing, specified through [add_recipe()], or the model specification
#' to fit, specified through [add_model()].
#'
#' @examples
#' library(recipes)
#'
#' rec <- recipe(mpg ~ cyl, mtcars)
#' rec <- step_log(rec, cyl)
#'
#' wrk <- workflow()
#' wrk <- add_recipe(wrk, rec)
#'
#' @export
workflow <- function() {
  new_workflow()
}

# ------------------------------------------------------------------------------

new_workflow <- function(pre = new_stage_pre(),
                         fit = new_stage_fit(),
                         post = new_stage_post(),
                         run = FALSE) {
  if (!is_stage(pre)) {
    abort("`pre` must be a `stage`.")
  }

  if (!is_stage(fit)) {
    abort("`fit` must be a `stage`.")
  }

  if (!is_stage(post)) {
    abort("`post` must be a `stage`.")
  }

  if (!is_scalar_logical(run)) {
    abort("`run` must be a single logical value.")
  }

  data <- list(
    pre = pre,
    fit = fit,
    post = post,
    run = run
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
  cat_line("")
  print_model(x)
  print_postprocessor(x)
  invisible(x)
}

print_header <- function(x) {
  if (x$run) {
    fit <- " [fit]"
  } else {
    fit <- ""
  }

  cat_line(glue::glue("<workflow{fit}>"))

  invisible(x)
}

print_preprocessor <- function(x) {
  actions <- names(x$pre$actions)
  preprocessor <- tab("<preprocessor>")

  if ("formula" %in% actions) {
    type <- tab("Formula", 2L)
  } else if ("recipe" %in% actions) {
    type <- tab("Recipe", 2L)
  } else {
    type <- tab("None", 2L)
  }

  preprocessor <- c(preprocessor, type)

  cat_line(preprocessor)
  invisible(x)
}

print_model <- function(x) {
  actions <- names(x$fit$actions)
  model <- tab("<model>")

  has_model <- "model" %in% actions

  if (!has_model) {
    model <- c(model, tab("None", 2L))
    cat_line(model)
    return(invisible(x))
  }

  # capture.output() is ugly but it works
  spec <- x$fit$actions$model$spec
  spec_format <- capture.output(spec)
  spec_format <- tab(spec_format, 2L)

  model <- c(model, spec_format)

  cat_line(model)
  invisible(x)
}

# Nothing for now
print_postprocessor <- function(x) {
  invisible(x)
}

tab <- function(x, times = 1L) {
  space <- paste0(rep("  ", times = times), collapse = "")
  paste0(space, x)
}

cat_line <- function (..., file = stdout()) {
  out <- paste0(..., collapse = "\n")
  cat(out, "\n", sep = "", file = file, append = TRUE)
}
