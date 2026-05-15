ugplot_test_internal <- function(name) {
  namespace <- tryCatch(asNamespace("ugplot"), error = function(e) NULL)
  if (!is.null(namespace) && exists(name, envir = namespace, inherits = FALSE)) {
    return(get(name, envir = namespace, inherits = FALSE))
  }
  if (exists(name, inherits = TRUE)) {
    return(get(name, inherits = TRUE))
  }
  stop("Could not find internal object: ", name, call. = FALSE)
}

ugplot_test_local_namespace_binding <- function(name, value, envir = parent.frame()) {
  defer_env <- envir
  namespace <- asNamespace("ugplot")
  old_value <- get(name, envir = namespace, inherits = FALSE)
  was_locked <- bindingIsLocked(name, namespace)

  if (was_locked) {
    unlockBinding(name, namespace)
  }
  assign(name, value, envir = namespace)
  if (was_locked) {
    lockBinding(name, namespace)
  }

  withr::defer({
    if (was_locked) {
      unlockBinding(name, namespace)
    }
    assign(name, old_value, envir = namespace)
    if (was_locked) {
      lockBinding(name, namespace)
    }
  }, envir = defer_env)

  invisible(value)
}
