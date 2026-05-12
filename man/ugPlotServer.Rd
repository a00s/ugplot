\name{ugPlotServer}
\alias{ugPlotServer}
\title{Start a ugplot job server}
\usage{
ugPlotServer(
  host = "127.0.0.1",
  port = 8080,
  jobs_dir = ugplot_default_jobs_dir(),
  token = Sys.getenv("UGPLOT_SERVER_TOKEN", unset = "")
)
}
\arguments{
\item{host}{Interface to bind. Use \code{"0.0.0.0"} for remote access.}

\item{port}{Port to listen on.}

\item{jobs_dir}{Directory used to persist datasets, status and results.}

\item{token}{Optional bearer token. If empty, requests are unauthenticated.}
}
\value{
The plumber server result.
}
\description{
Starts an HTTP server that can receive datasets, run jobs in background R
processes, report progress, and return completed results.
}
