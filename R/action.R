# ------------------------------------------------------------------------------

new_action_recipe <- function() {
  new_action(subclass = "action_recipe")
}

new_action_formula <- function() {
  new_action(subclass = "action_formula")
}

# ------------------------------------------------------------------------------

new_action_model <- function() {
  new_action(subclass = "action_model")
}

new_action_terms <- function() {
  new_action(subclass = "action_terms")
}

# ------------------------------------------------------------------------------

# An `action` is a list of objects that define how to perform a specific action,
# such as working with a recipe, or formula terms, or a model

new_action <- function(..., subclass = character()) {
  data <- list2(...)

  if (!is_uniquely_named(data)) {
    abort("All elements of `...` must be uniquely named.")
  }

  structure(data, class = c(subclass, "action"))
}

# ------------------------------------------------------------------------------

is_action <- function(x) {
  inherits(x, "action")
}
