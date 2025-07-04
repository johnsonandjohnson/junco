% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rbmi.R
\name{make_rbmi_cluster}
\alias{make_rbmi_cluster}
\title{Create a `rbmi` ready cluster}
\usage{
make_rbmi_cluster(cluster_or_cores = 1, objects = NULL, packages = NULL)
}
\arguments{
\item{cluster_or_cores}{Number of parallel processes to use or an existing cluster to make use of}

\item{objects}{a named list of objects to export into the sub-processes}

\item{packages}{a character vector of libraries to load in the sub-processes

This function is a wrapper around `parallel::makePSOCKcluster()` but takes
care of configuring `rbmi` to be used in the sub-processes as well as loading
user defined objects and libraries and setting the seed for reproducibility.

If `cluster_or_cores` is `1` this function will return `NULL`.

If `cluster_or_cores` is a cluster created via `parallel::makeCluster()` then this function
just takes care of inserting the relevant `rbmi` objects into the existing cluster.}
}
\description{
Create a `rbmi` ready cluster
}
\examples{
# Basic usage # dontrun because .check_ncores
\dontrun{
make_rbmi_cluster(5)
closeAllConnections()

# User objects + libraries
VALUE <- 5
myfun <- function(x) {
  x + day(VALUE) # From lubridate::day()
}
make_rbmi_cluster(5, list(VALUE = VALUE, myfun = myfun), c('lubridate'))
closeAllConnections()

# Using a already created cluster
cl <- parallel::makeCluster(5)
make_rbmi_cluster(cl)
closeAllConnections()
}
}
