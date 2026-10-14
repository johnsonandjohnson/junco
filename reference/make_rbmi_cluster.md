# Create a `rbmi` ready cluster

**\[deprecated\]** Use
[`rbmi::make_rbmi_cluster()`](https://openpharma.github.io/rbmi/latest-tag/reference/make_rbmi_cluster.html)
instead.

## Usage

``` r
make_rbmi_cluster(cluster_or_cores = 1, objects = NULL, packages = NULL)
```

## Arguments

- cluster_or_cores:

  (`integer` or `cluster object`)  
  Number of parallel processes to use or an existing cluster to make use
  of

- objects:

  (`list`)  
  A named list of objects to export into the sub-processes

- packages:

  (`character vector`)  
  A character vector of libraries to load in the sub-processes

## Value

See
[`rbmi::make_rbmi_cluster()`](https://openpharma.github.io/rbmi/latest-tag/reference/make_rbmi_cluster.html).
