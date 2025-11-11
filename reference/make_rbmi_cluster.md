# Create a `rbmi` ready cluster

Create a `rbmi` ready cluster

## Usage

``` r
make_rbmi_cluster(cluster_or_cores = 1, objects = NULL, packages = NULL)
```

## Arguments

- cluster_or_cores:

  Number of parallel processes to use or an existing cluster to make use
  of

- objects:

  a named list of objects to export into the sub-processes

- packages:

  a character vector of libraries to load in the sub-processes

  This function is a wrapper around
  [`parallel::makePSOCKcluster()`](https://rdrr.io/r/parallel/makeCluster.html)
  but takes care of configuring `rbmi` to be used in the sub-processes
  as well as loading user defined objects and libraries and setting the
  seed for reproducibility.

## Value

If `cluster_or_cores` is `1` this function will return `NULL`. If
`cluster_or_cores` is a number greater than `1`, a cluster with
`cluster_or_cores` cores is returned.

If `cluster_or_cores` is a cluster created via
[`parallel::makeCluster()`](https://rdrr.io/r/parallel/makeCluster.html)
then this function returns it after inserting the relevant `rbmi`
objects into the existing cluster.

## Examples

``` r
if (FALSE) { # \dontrun{
make_rbmi_cluster(5)
closeAllConnections()

VALUE <- 5
myfun <- function(x) {
  x + day(VALUE)
}
make_rbmi_cluster(5, list(VALUE = VALUE, myfun = myfun), c("lubridate"))
closeAllConnections()

cl <- parallel::makeCluster(5)
make_rbmi_cluster(cl)
closeAllConnections()
} # }
```
