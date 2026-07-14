ugplot_server_r_packages <- function() {
  c("callr", "httr", "jsonlite", "plumber")
}

ugplot_server_geo_r_packages <- function() {
  c(
    "GEOquery",
    "minfi",
    "IlluminaHumanMethylation450kanno.ilmn12.hg19",
    "IlluminaHumanMethylationEPICanno.ilm10b4.hg19",
    "ensembldb",
    "EnsDb.Hsapiens.v75",
    "GenomicRanges",
    "IRanges",
    "S4Vectors",
    "sesame",
    "sesameData"
  )
}

ugplot_installed_r_packages <- function() {
  rownames(utils::installed.packages())
}

ugplot_model_dependency_status <- function(models = NULL, exclude_models = character()) {
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required to inspect model dependencies.", call. = FALSE)
  }

  all_models <- caret::getModelInfo()
  model_names <- names(all_models)
  if (!is.null(models)) {
    models <- unique(as.character(models))
    unknown_models <- setdiff(models, model_names)
    model_names <- intersect(models, model_names)
  } else {
    unknown_models <- character(0)
  }
  model_names <- setdiff(model_names, exclude_models)
  installed_packages <- ugplot_installed_r_packages()

  model_rows <- lapply(model_names, function(model_name) {
    libraries <- all_models[[model_name]]$library
    if (is.null(libraries)) {
      libraries <- character(0)
    }
    libraries <- unique(as.character(libraries))
    missing_libraries <- setdiff(libraries, installed_packages)
    data.frame(
      model = model_name,
      packages = paste(libraries, collapse = ", "),
      missing_packages = paste(missing_libraries, collapse = ", "),
      installed = length(missing_libraries) == 0,
      stringsAsFactors = FALSE
    )
  })
  models_table <- if (length(model_rows) > 0) {
    do.call(rbind, model_rows)
  } else {
    data.frame(
      model = character(0),
      packages = character(0),
      missing_packages = character(0),
      installed = logical(0),
      stringsAsFactors = FALSE
    )
  }

  missing_by_model <- models_table[!models_table$installed, c("model", "missing_packages"), drop = FALSE]
  packages_to_install <- unique(unlist(strsplit(missing_by_model$missing_packages, ",[[:space:]]*")))
  packages_to_install <- packages_to_install[nzchar(packages_to_install)]

  list(
    models_installed = models_table$model[models_table$installed],
    models_missing = models_table$model[!models_table$installed],
    packages_to_install = packages_to_install,
    missing_by_model = missing_by_model,
    models = models_table,
    unknown_models = unknown_models
  )
}

ugplot_print_model_dependency_status <- function(status) {
  message(
    "caret models installed: ", length(status$models_installed),
    " | missing dependencies: ", length(status$models_missing)
  )
  if (length(status$packages_to_install) > 0) {
    message("Packages to install: ", paste(status$packages_to_install, collapse = ", "))
  } else {
    message("All inspected caret model dependencies are available.")
  }
  if (length(status$unknown_models) > 0) {
    message("Unknown caret models ignored: ", paste(status$unknown_models, collapse = ", "))
  }
  invisible(status)
}

#' Check caret model dependencies used by ugPlot
#'
#' Lists caret models whose required R packages are not installed in the
#' current R library.
#'
#' @param models Optional character vector of caret model names. Defaults to all
#'   caret models.
#' @param exclude_models Optional character vector of caret model names to skip.
#' @return Invisibly returns dependency status tables and package names.
#' @export
ugPlotCheckModelDeps <- function(models = NULL, exclude_models = character()) {
  status <- ugplot_model_dependency_status(models = models, exclude_models = exclude_models)
  ugplot_print_model_dependency_status(status)
  invisible(status)
}

#' Install caret model dependencies used by ugPlot
#'
#' Installs the R packages required by missing caret models in the current R
#' library. Run this on the machine that will execute the jobs, including the
#' ugPlot server host.
#'
#' @param models Optional character vector of caret model names. Defaults to all
#'   caret models.
#' @param install Whether to install missing packages.
#' @param dependencies Passed to \code{install.packages()}.
#' @param exclude_models Optional character vector of caret model names to skip.
#' @return Invisibly returns dependency status after the attempted installation.
#' @export
ugPlotInstallModelDeps <- function(models = NULL, install = TRUE, dependencies = TRUE,
                                   exclude_models = character()) {
  status <- ugplot_model_dependency_status(models = models, exclude_models = exclude_models)
  ugplot_print_model_dependency_status(status)

  if (install && length(status$packages_to_install) > 0) {
    utils::install.packages(status$packages_to_install, dependencies = dependencies)
    status <- ugplot_model_dependency_status(models = models, exclude_models = exclude_models)
    ugplot_print_model_dependency_status(status)
  }

  invisible(status)
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
#' @param install_model_deps Whether to install packages required by missing
#'   caret models too. Defaults to \code{FALSE} because this can install many
#'   packages.
#' @param install_geo_deps Whether to install optional Bioconductor packages
#'   required for GEO methylation annotation and raw IDAT/sesame workflows.
#' @return Invisibly returns a list with system and R dependency status.
#' @export
ugPlotInstallServerDeps <- function(install = TRUE, dependencies = TRUE,
                                    install_model_deps = FALSE,
                                    install_geo_deps = TRUE) {
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
  if (isTRUE(install_geo_deps)) {
    r_packages <- unique(c(r_packages, ugplot_server_geo_r_packages()))
  }
  r_missing <- r_packages[!vapply(r_packages, requireNamespace, logical(1), quietly = TRUE)]
  if (install && length(r_missing) > 0) {
    if (isTRUE(install_geo_deps)) {
      if (!requireNamespace("BiocManager", quietly = TRUE)) {
        utils::install.packages("BiocManager", dependencies = dependencies)
      }
      bioc_dependencies <- if (isTRUE(dependencies)) {
        c("Depends", "Imports", "LinkingTo")
      } else {
        dependencies
      }
      BiocManager::install(r_missing, ask = FALSE, update = FALSE, dependencies = bioc_dependencies)
    } else {
      utils::install.packages(r_missing, dependencies = dependencies)
    }
    r_missing <- r_packages[!vapply(r_packages, requireNamespace, logical(1), quietly = TRUE)]
  }

  if (length(r_missing) > 0) {
    message("Missing R packages: ", paste(r_missing, collapse = ", "))
  } else {
    message("ugPlotServer dependencies are available.")
  }

  model_deps <- NULL
  if (isTRUE(install_model_deps)) {
    model_deps <- ugPlotInstallModelDeps(install = install, dependencies = dependencies)
  }

  invisible(list(system_missing = character(0), r_missing = r_missing, model_deps = model_deps))
}
