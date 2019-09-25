is_uniquely_named <- function(x) {
  if (vec_size(x) > 0) {
    is_named(x) && !anyDuplicated(names(x))
  } else {
    TRUE
  }
}
