local_lifecycle_quiet <- function(frame = caller_env()) {
  local_options(lifecycle_verbosity = "quiet", .frame = frame)
}
