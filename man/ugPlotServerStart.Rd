\name{ugPlotServerStart}
\alias{ugPlotServerStart}
\title{Start a ugplot job server in the background}
\usage{
ugPlotServerStart(
  host = "127.0.0.1",
  port = 8080,
  jobs_dir = ugplot_default_jobs_dir(),
  token = "",
  name = "default"
)
}
\arguments{
\item{host}{Interface to bind.}

\item{port}{Port to listen on.}

\item{jobs_dir}{Directory used to persist datasets, status and results.}

\item{token}{Optional bearer token. Defaults to no authentication.}

\item{name}{Local server handle name used by status/stop.}
}
\value{
Invisibly returns the server state.
}
\description{
Starts \code{ugPlotServer()} in a detached R process and immediately returns
control to the console.
}
