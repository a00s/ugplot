\name{ugPlotServerStop}
\alias{ugPlotServerStop}
\title{Stop a background ugplot job server}
\usage{
ugPlotServerStop(name = "default", port = NULL)
}
\arguments{
\item{name}{Local server handle name.}

\item{port}{Optional port used to discover and stop a server when no live
state file is available. Defaults to the saved state port, or 8080 when there
is no state.}
}
\value{
Invisibly returns TRUE when a process was stopped.
}
\description{
Stop a background ugplot job server.
}
