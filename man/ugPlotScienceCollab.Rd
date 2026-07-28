\name{ugPlotScienceCollab}
\alias{ugPlotScienceCollab}
\title{Run a headless Science Collab client}
\usage{
ugPlotScienceCollab(
  coordinator,
  scientist_name,
  cpu_limit = max(1L, parallel::detectCores() - 1L),
  install_model_deps = TRUE,
  install_client_deps = TRUE,
  poll_seconds = 6,
  max_missions = Inf
)
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

\item{max_missions}{Maximum missions to complete before returning. The default
keeps the client running until interrupted.}
}
\value{
Invisibly returns contribution counters when the client stops.
}
\description{
Connects this machine to the public collaboration endpoint of an ugPlot
coordinator. The client can claim and complete published Science Collab
missions, but it does not receive or use the coordinator's administrative
bearer token and therefore cannot submit arbitrary remote jobs.
}
