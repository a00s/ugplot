\name{ugPlotServerStatus}
\alias{ugPlotServerStatus}
\title{Get background ugplot job server status}
\usage{
ugPlotServerStatus(name = "default", port = NULL)
}
\arguments{
\item{name}{Local server handle name.}

\item{port}{Optional port used to discover a server when no live state file is
available. Defaults to the saved state port, or 8080 when there is no state.}
}
\value{
A list with server metadata and running state.
}
\description{
Get background ugplot job server status.
}
