\name{ugPlot}
\alias{ugPlot}
\title{Launch the ugplot Shiny application}
\usage{
ugPlot(dataset = data.frame())
}
\arguments{
  \item{dataset}{A data frame to pre-load in the app. If empty, the app starts without a dataset.}
}
\value{
A \code{shiny.appobj} that runs the ugplot interface.
}
\description{
Starts the ugplot Shiny interface used to explore data relationships with heatmaps,
2D plots and predictive modeling workflows.
}
\examples{
if (interactive()) {
  ugPlot()
}
}
