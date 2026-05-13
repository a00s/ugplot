\name{ugPlotInstallModelDeps}
\alias{ugPlotInstallModelDeps}
\title{Install caret model dependencies used by ugPlot}
\usage{
ugPlotInstallModelDeps(
  models = NULL,
  install = TRUE,
  dependencies = TRUE,
  exclude_models = character()
)
}
\arguments{
\item{models}{Optional character vector of caret model names. Defaults to all
caret models.}

\item{install}{Whether to install missing packages.}

\item{dependencies}{Passed to \code{install.packages()}.}

\item{exclude_models}{Optional character vector of caret model names to skip.}
}
\value{
Invisibly returns dependency status after the attempted installation.
}
\description{
Installs the R packages required by missing caret models in the current R
library. Run this on the machine that will execute the jobs, including the
ugPlot server host.
}
