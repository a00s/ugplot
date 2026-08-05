\name{ugPlotScienceCollabStart}
\alias{ugPlotScienceCollabStart}
\alias{ugPlotScienceCollabStatus}
\alias{ugPlotScienceCollabStop}
\title{Manage a background Science Collab client}
\usage{
ugPlotScienceCollabStart(
  coordinator,
  scientist_name,
  cpu_limit = max(1L, parallel::detectCores() - 1L),
  install_model_deps = TRUE,
  install_client_deps = TRUE,
  poll_seconds = 6,
  name = "default"
)

ugPlotScienceCollabStatus(name = "default")

ugPlotScienceCollabStop(name = "default")
}
\arguments{
\item{coordinator}{Coordinator IP, hostname, or HTTP(S) URL. Bare hosts use
port 8080.}

\item{scientist_name}{Public name shown for this contributor.}

\item{cpu_limit}{Maximum CPU cores offered to a mission.}

\item{install_model_deps}{Whether to attempt installation of dependencies for
every missing caret model before connecting.}

\item{install_client_deps}{Whether to install the small set of packages needed
by the headless client.}

\item{poll_seconds}{Seconds between attempts when no compatible mission is
waiting.}

\item{name}{Local handle used by the start, status, and stop functions.}
}
\value{
\code{ugPlotScienceCollabStart()} and \code{ugPlotScienceCollabStatus()}
invisibly return the client state. \code{ugPlotScienceCollabStop()} invisibly
returns whether a running client was stopped.
}
\description{
Starts, inspects, or stops a detached Science Collab client. The package keeps
the process ID, configuration, and log location under
\code{~/.ugplot/science-collab}.
}
