# Split Function Helper

A function which aids the construction for users to create their own
split function for combined columns

## Usage

``` r
make_combo_splitfun(nm, label = nm, levels = NULL, rm_other_facets = TRUE)
```

## Arguments

- nm:

  character(1). Name/virtual 'value' for the new facet

- label:

  character(1). label for the new facet

- levels:

  character or NULL. The levels to combine into the new facet, or NULL,
  indicating the facet should include all incoming data.

- rm_other_facets:

  logical(1). Should facets other than the newly created one be removed.
  Defaults to `TRUE`

## Value

function usable directly as a split function.

## Examples

``` r
aesevall_spf <- make_combo_splitfun(nm = 'AESEV_ALL', label  = 'Any AE', levels = NULL)
```
