\name{ugPlotCheckModelDeps}
\alias{ugPlotCheckModelDeps}
\title{Check caret model dependencies used by ugPlot}
\usage{
ugPlotCheckModelDeps(models = NULL, exclude_models = character())
}
\arguments{
\item{models}{Optional character vector of caret model names. Defaults to all
caret models.}

\item{exclude_models}{Optional character vector of caret model names to skip.}
}
\value{
Invisibly returns dependency status tables and package names.
}
\description{
Lists caret models whose required R packages are not installed in the
current R library.
}
