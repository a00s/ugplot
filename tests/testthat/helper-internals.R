ugplot_test_internal <- function(name) {
  namespace <- tryCatch(asNamespace("ugplot"), error = function(e) NULL)
  if (!is.null(namespace) && exists(name, envir = namespace, inherits = FALSE)) {
    return(get(name, envir = namespace, inherits = FALSE))
  }
  get(name, inherits = TRUE)
}
