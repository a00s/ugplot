ugplot_server_r_packages <- function() {
  c("callr", "httr", "jsonlite", "plumber")
}

ugplot_command_exists <- function(command) {
  nzchar(Sys.which(command))
}

ugplot_has_header <- function(header, include_dirs = character()) {
  include_dirs <- unique(c(
    include_dirs,
    "/usr/include",
    "/usr/local/include",
    "/opt/homebrew/include",
    "/opt/local/include"
  ))
  any(file.exists(file.path(include_dirs, header)))
}

ugplot_has_pkg_config_package <- function(package) {
  if (!ugplot_command_exists("pkg-config")) {
    return(FALSE)
  }
  result <- system2("pkg-config", c("--exists", package), stdout = FALSE, stderr = FALSE)
  identical(result, 0L)
}

ugplot_detect_linux_package_manager <- function() {
  if (ugplot_command_exists("dnf")) {
    return("dnf")
  }
  if (ugplot_command_exists("yum")) {
    return("yum")
  }
  if (ugplot_command_exists("apt-get")) {
    return("apt-get")
  }
  if (ugplot_command_exists("zypper")) {
    return("zypper")
  }
  NA_character_
}

ugplot_server_system_dependency_commands <- function() {
  os <- Sys.info()[["sysname"]]
  if (identical(os, "Darwin")) {
    return(c("brew install libsodium"))
  }
  if (identical(os, "Linux")) {
    manager <- ugplot_detect_linux_package_manager()
    if (identical(manager, "dnf")) {
      return(c("sudo dnf install libsodium-devel"))
    }
    if (identical(manager, "yum")) {
      return(c("sudo yum install libsodium-devel"))
    }
    if (identical(manager, "apt-get")) {
      return(c("sudo apt-get update", "sudo apt-get install libsodium-dev"))
    }
    if (identical(manager, "zypper")) {
      return(c("sudo zypper install libsodium-devel"))
    }
  }
  c("Install the libsodium development package for your operating system.")
}

ugplot_missing_server_system_deps <- function() {
  missing <- character(0)
  if (!ugplot_has_pkg_config_package("libsodium") && !ugplot_has_header("sodium.h")) {
    missing <- c(missing, "libsodium development headers")
  }
  missing
}

ugplot_assert_server_system_deps <- function() {
  missing <- ugplot_missing_server_system_deps()
  if (length(missing) == 0) {
    return(invisible(TRUE))
  }
  commands <- paste0("  ", ugplot_server_system_dependency_commands(), collapse = "\n")
  stop(
    paste0(
      "Missing system dependency for ugPlotServer(): ",
      paste(missing, collapse = ", "),
      "\nInstall it first, then rerun ugPlotInstallServerDeps():\n",
      commands
    ),
    call. = FALSE
  )
}

#' Install ugplot server dependencies
#'
#' Checks system dependencies required by the remote job server and installs
#' the optional R packages used by \code{ugPlotServer()}.
#'
#' @param install Whether to install missing R packages.
#' @param dependencies Passed to \code{install.packages()}.
#' @return Invisibly returns a list with system and R dependency status.
#' @export
ugPlotInstallServerDeps <- function(install = TRUE, dependencies = TRUE) {
  system_missing <- ugplot_missing_server_system_deps()
  if (length(system_missing) > 0) {
    commands <- paste0("  ", ugplot_server_system_dependency_commands(), collapse = "\n")
    message(
      "Missing system dependency: ", paste(system_missing, collapse = ", "), "\n",
      "Run:\n", commands, "\n",
      "Then rerun ugPlotInstallServerDeps()."
    )
    return(invisible(list(system_missing = system_missing, r_missing = ugplot_server_r_packages())))
  }

  r_packages <- ugplot_server_r_packages()
  r_missing <- r_packages[!vapply(r_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (install && length(r_missing) > 0) {
    utils::install.packages(r_missing, dependencies = dependencies)
    r_missing <- r_packages[!vapply(r_packages, requireNamespace, logical(1), quietly = TRUE)]
  }

  if (length(r_missing) > 0) {
    message("Missing R packages: ", paste(r_missing, collapse = ", "))
  } else {
    message("ugPlotServer dependencies are available.")
  }

  invisible(list(system_missing = character(0), r_missing = r_missing))
}
