check_tunable <- function(x) {
  expect_equal(
    names(x),
    c("name", "call_info", "source", "component", "component_id")
  )
  expect_equal(class(x$name), "character")
  expect_equal(class(x$call_info), "list")
  expect_equal(class(x$source), "character")
  expect_equal(class(x$component), "character")
  expect_equal(class(x$component_id), "character")

  for (i in seq_along(x$call_info)) {
    check_call_info(x$call_info[[i]])
  }

  invisible(TRUE)
}

check_call_info <- function(x) {
  if (all(is.null(x))) {
    # it is possible that engine parameter do not have call info
    return(invisible(TRUE))
  }
  expect_true(all(c("pkg", "fun") %in% names(x)))
  expect_equal(class(x$pkg), "character")
  expect_equal(class(x$fun), "character")
  invisible(TRUE)
}
