.onLoad <- function(libname, pkgname) {
  settings <- list(
    # A named list for tracking table names and counts in `insert_blank_line()`
    junco.insert_blank_line = list()
  )

  to_set <- !names(settings) %in% names(options())
  if (any(to_set)) options(settings[to_set])
  invisible()
}


.onUnload <- function(libpath) {
  settings <- list(
    junco.insert_blank_line = NULL
  )

  options(settings)
  invisible()
}
