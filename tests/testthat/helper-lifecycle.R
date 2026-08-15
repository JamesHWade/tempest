quiet_source_store <- function(...) {
  withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    SourceStore$new(...)
  )
}
