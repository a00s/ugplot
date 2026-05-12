ugplot_test_internal <- function(name) {
  if (exists(name, inherits = TRUE)) {
    return(get(name, inherits = TRUE))
  }
  namespace <- tryCatch(asNamespace("ugplot"), error = function(e) NULL)
  if (!is.null(namespace) && exists(name, envir = namespace, inherits = FALSE)) {
    return(get(name, envir = namespace, inherits = FALSE))
  }
  stop("Could not find internal object: ", name, call. = FALSE)
}
