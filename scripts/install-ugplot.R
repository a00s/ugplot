# Non-interactive ugPlot installer for a clean R library.
# Run with:
# source("https://raw.githubusercontent.com/a00s/ugplot/main/scripts/install-ugplot.R")

options(
  repos = c(
    ugplot = "https://a00s.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
  )
)

user_library <- path.expand(Sys.getenv("R_LIBS_USER", unset = ""))
if (nzchar(user_library)) {
  dir.create(user_library, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(user_library)) .libPaths(c(user_library, .libPaths()))
}

install_if_missing <- function(packages, repos = getOption("repos")) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0L) {
    detected_cpus <- suppressWarnings(as.integer(parallel::detectCores(logical = TRUE)))
    if (is.na(detected_cpus) || detected_cpus < 1L) detected_cpus <- 1L
    install.packages(
      missing,
      repos = repos,
      dependencies = NA,
      Ncpus = max(1L, detected_cpus - 1L)
    )
  }
  still_missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still_missing) > 0L) {
    stop("Could not install: ", paste(still_missing, collapse = ", "), call. = FALSE)
  }
  invisible(packages)
}

message("Preparing CRAN and Bioconductor repositories...")
install_if_missing(c("BiocManager", "remotes"))

bioc_repositories <- BiocManager::repositories()
all_repositories <- c(
  ugplot = "https://a00s.r-universe.dev",
  bioc_repositories,
  CRAN = "https://cloud.r-project.org"
)
all_repositories <- all_repositories[!duplicated(unname(all_repositories))]
options(repos = all_repositories)

if (!requireNamespace("ConsensusClusterPlus", quietly = TRUE)) {
  message("Installing required Bioconductor package ConsensusClusterPlus...")
  BiocManager::install(
    "ConsensusClusterPlus",
    ask = FALSE,
    update = FALSE,
    dependencies = NA
  )
}
if (!requireNamespace("ConsensusClusterPlus", quietly = TRUE)) {
  stop("ConsensusClusterPlus could not be installed.", call. = FALSE)
}

ref <- Sys.getenv("UGPLOT_INSTALL_REF", unset = "main")
message("Installing ugPlot from GitHub ref ", ref, "...")
remotes::install_github(
  paste0("a00s/ugplot@", ref),
  dependencies = NA,
  upgrade = "never",
  force = TRUE,
  repos = all_repositories
)

if (!requireNamespace("ugplot", quietly = TRUE)) {
  stop("ugplot installation did not complete.", call. = FALSE)
}
message("ugplot ", as.character(utils::packageVersion("ugplot")), " installed successfully.")
