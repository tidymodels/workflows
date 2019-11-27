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
                         trained = FALSE) {
  if (!is_stage(pre)) {
    abort("`pre` must be a `stage`.")
  }

  if (!is_stage(fit)) {
    abort("`fit` must be a `stage`.")
  }

  if (!is_stage(post)) {
    abort("`post` must be a `stage`.")
  }

  if (!is_scalar_logical(trained)) {
    abort("`trained` must be a single logical value.")
  }

  data <- list(
    pre = pre,
    fit = fit,
    post = post,
    trained = trained
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
  if (x$trained) {
    trained <- " [trained]"
  } else {
    trained <- ""
  }

  cat(cli::rule(glue::glue("model workflow {trained}")))

  invisible(x)
}

print_preprocessor <- function(x) {
  actions <- names(x$pre$actions)

  if ("formula" %in% actions) {
    cat_line(cli::style_bold("\nformula\n"))
    chr_f <- rlang::expr_text(x$pre$actions$formula$formula)
    cat_line(glue::glue_collapse(chr_f, width = options()$width - 4))
  } else if ("recipe" %in% actions) {
    opers <- x$pre$actions$recipe$recipe$steps
    if (length(opers) > 0) {
      opers <- vapply(opers, function(x) class(x)[1], character(1))
    } else {
      opers <- ("no steps\n")
    }
    cat_line(cli::style_bold("\nrecipe\n"))
    cat_line(glue::glue_collapse(opers, sep = ", ", last = ", and ", width = options()$width - 4))
  } else {
    cat("\nno pre-processors\n")
  }

  invisible(x)
}

print_model <- function(x) {
  cat_line(cli::style_bold("model\n"))

  actions <- names(x$fit$actions)

  has_model <- "model" %in% actions

  if (!has_model) {
    cat_line("no model object")
    return(invisible(x))
  }

  if (!is.null(x$fit$fit)) {
    print(x$fit$fit$fit)
  } else {
    print(x$fit$actions$model$spec)
  }
  invisible(x)
}

# Nothing for now
print_postprocessor <- function(x) {
  invisible(x)
}

cat_line <- function(...) {
  cat(paste0(..., collapse = "\n"), "\n", sep = "")
}
