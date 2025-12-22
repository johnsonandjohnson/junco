# Conditional Removal of Facets

Conditional Removal of Facets

## Usage

``` r
cond_rm_facets(
  facets = NULL,
  facets_regex = NULL,
  ancestor_pos = 1,
  split = NULL,
  split_regex = NULL,
  value = NULL,
  value_regex = NULL,
  keep_matches = FALSE
)
```

## Arguments

- facets:

  (`character` or NULL)  
  Vector of facet names to be removed if condition(s) are met

- facets_regex:

  (`character`)  
  Regular expression to identify facet names to be removed if
  condition(s) are met.

- ancestor_pos:

  (`numeric`)  
  Row in spl_context to check the condition within. E.g., 1 represents
  the first split, 2 represents the second split nested within the
  first, etc. NA specifies that the conditions should be checked at all
  split levels. Negative integers indicate position counting back from
  the current one, e.g., -1 indicates the direct parent (most recent
  split before this one). Negative and positive/NA positions cannot be
  mixed.

- split:

  (`character` or NULL)  
  If specified, name of the split at position `ancestor_pos` must be
  identical to this value for the removal condition to be met.

- split_regex:

  (`character` or NULL)  
  If specified, a regular expression the name of the split at position
  `ancestor_pos` must match for the removal condition to be met. Cannot
  be specified at the same time as `split`.

- value:

  (`character` or NULL)  
  If specified, split (facet) value at position `ancestor_pos` must be
  identical to this value for removal condition to be met.

- value_regex:

  (`character` or NULL)  
  If specified, a regular expression the value of the split at position
  `ancestor_pos` must match for the removal condition to be met. Cannot
  be specified at the same time as `value`.

- keep_matches:

  (`logical`)  
  Given the specified condition is met, should the facets removed be
  those matching `facets`/`facets_regex` (`FALSE`, the default), or
  those *not* matching (`TRUE`).

## Value

A function suitable for use in `make_split_fun`'s `post` argument which
encodes the specified condition.

## Details

Facet removal occurs when the specified condition(s) on the split(s) and
or value(s) are met within at least one of the split_context rows
indicated by `ancestor_pos`; otherwise the set of facets is returned
unchanged.

If facet removal is performed, either *all* facets which match `facets`
(or `facets_regex` will be removed ( the default `keep_matches == FALSE`
case), or all *non-matching* facets will be removed (when
`keep_matches_only == TRUE`).

## Note

A degenerate table is likely to be returned if all facets are removed.

## Examples

``` r
rm_a_from_placebo <- cond_rm_facets(
  facets = "A",
  ancestor_pos = NA,
  value_regex = "Placeb",
  split = "ARM"
)
mysplit <- make_split_fun(post = list(rm_a_from_placebo))

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STRATA1", split_fun = mysplit) |>
  analyze("AGE", mean, format = "xx.x")
build_table(lyt, ex_adsl)
#>            A: Drug X         B: Placebo        C: Combination   
#>         A      B      C       B       C       A       B      C  
#> ————————————————————————————————————————————————————————————————
#> mean   33.1   33.9   34.2   36.0    35.2    34.2    36.3    35.6

rm_bc_from_combo <- cond_rm_facets(
  facets = c("B", "C"),
  ancestor_pos = -1,
  value_regex = "Combi"
)
mysplit2 <- make_split_fun(post = list(rm_bc_from_combo))

lyt2 <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STRATA1", split_fun = mysplit2) |>
  analyze("AGE", mean, format = "xx.x")
tbl2 <- build_table(lyt2, ex_adsl)
tbl2
#>            A: Drug X            B: Placebo       C: Combination
#>         A      B      C      A      B      C           A       
#> ———————————————————————————————————————————————————————————————
#> mean   33.1   33.9   34.2   35.1   36.0   35.2        34.2     

rm_bc_from_combo2 <- cond_rm_facets(
  facets_regex = "^A$",
  ancestor_pos = -1,
  value_regex = "Combi",
  keep_matches = TRUE
)
mysplit3 <- make_split_fun(post = list(rm_bc_from_combo2))

lyt3 <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STRATA1", split_fun = mysplit3) |>
  analyze("AGE", mean, format = "xx.x")
tbl3 <- build_table(lyt3, ex_adsl)

stopifnot(identical(cell_values(tbl2), cell_values(tbl3)))
```
