ugplot_build_version <- function() {
  "20260716.1"
}

ugplot_compare_build_versions <- function(local_version, remote_version) {
  local_version <- if (is.null(local_version)) "" else as.character(local_version)
  remote_version <- if (is.null(remote_version)) "" else as.character(remote_version)
  if (!nzchar(local_version) || !nzchar(remote_version)) {
    return(NA_integer_)
  }
  parse_version <- function(value) {
    parts <- strsplit(value, ".", fixed = TRUE)[[1]]
    suppressWarnings(as.integer(parts))
  }
  local_parts <- parse_version(local_version)
  remote_parts <- parse_version(remote_version)
  max_length <- max(length(local_parts), length(remote_parts))
  local_parts <- c(local_parts, rep(0L, max_length - length(local_parts)))
  remote_parts <- c(remote_parts, rep(0L, max_length - length(remote_parts)))
  if (any(is.na(local_parts)) || any(is.na(remote_parts))) {
    return(NA_integer_)
  }
  for (i in seq_len(max_length)) {
    if (local_parts[[i]] > remote_parts[[i]]) {
      return(1L)
    }
    if (local_parts[[i]] < remote_parts[[i]]) {
      return(-1L)
    }
  }
  0L
}

ugplot_version_mismatch_message <- function(local_version, remote_version) {
  local_version <- if (is.null(local_version)) "" else as.character(local_version)
  remote_version <- if (is.null(remote_version)) "" else as.character(remote_version)
  if (!nzchar(remote_version)) {
    return(paste0(
      "Version mismatch: interface ", local_version,
      ", remote server did not report a version. Update the remote server to ",
      local_version, "."
    ))
  }
  comparison <- ugplot_compare_build_versions(local_version, remote_version)
  update_target <- if (is.na(comparison) || comparison > 0L) {
    paste0("Update the remote server to ", local_version, ".")
  } else if (comparison < 0L) {
    paste0("Update this interface to ", remote_version, ".")
  } else {
    ""
  }
  paste0(
    "Version mismatch: interface ", local_version,
    ", remote server ", remote_version, ". ",
    update_target
  )
}
