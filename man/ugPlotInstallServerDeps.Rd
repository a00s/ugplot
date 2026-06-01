\name{ugPlotInstallServerDeps}
\alias{ugPlotInstallServerDeps}
\title{Install ugplot server dependencies}
\usage{
ugPlotInstallServerDeps(
  install = TRUE,
  dependencies = TRUE,
  install_model_deps = FALSE,
  install_geo_deps = TRUE
)
}
\arguments{
\item{install}{Whether to install missing R packages.}

\item{dependencies}{Passed to \code{install.packages()}.}

\item{install_model_deps}{Whether to install packages required by missing
caret models too. Defaults to \code{FALSE} because this can install many
packages.}

\item{install_geo_deps}{Whether to install optional Bioconductor packages
required for GEO methylation annotation and raw IDAT/sesame workflows.}
}
\value{
Invisibly returns a list with system and R dependency status.
}
\description{
Checks system dependencies required by the remote job server and installs
the optional R packages used by \code{ugPlotServer()}.
}
