\name{ugPlotServer}
\alias{ugPlotServer}
\title{Start a ugplot job server}
\usage{
ugPlotServer(
  host = "0.0.0.0",
  port = 8080,
  jobs_dir = ugplot_default_jobs_dir(),
  token = "",
  name = "default",
  register = TRUE
)
}
\arguments{
\item{host}{Interface to bind. Use \code{"0.0.0.0"} for remote access.}

\item{port}{Port to listen on.}

\item{jobs_dir}{Directory used to persist datasets, status and results.}

\item{token}{Optional bearer token. Defaults to no authentication.}

\item{name}{Local server handle name used by status/stop when the server is
started directly.}

\item{register}{Whether to write a local state file for status/stop.}
}
\value{
The plumber server result.
}
\description{
Starts an HTTP server that can receive datasets, run jobs in background R
processes, report progress, and return completed results.
}
