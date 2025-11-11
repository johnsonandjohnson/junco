# Safely Prune Table With Empty Table Message If Needed

Safely Prune Table With Empty Table Message If Needed

## Usage

``` r
safe_prune_table(
  tt,
  prune_func = prune_empty_level,
  stop_depth = NA,
  empty_msg = " - No Data To Display - ",
  spancols = FALSE
)
```

## Arguments

- tt:

  (`TableTree` or related class)  
  a `TableTree` object representing a populated table.

- prune_func:

  (`function`)  
  a function to be called on each subtree which returns `TRUE` if the
  entire subtree should be removed.

- stop_depth:

  (`numeric(1)`)  
  the depth after which subtrees should not be checked for pruning.
  Defaults to `NA` which indicates pruning should happen at all levels.

- empty_msg:

  character(1). The message to place in the table if no rows were left
  after pruning

- spancols:

  logical(1). Should `empty_msg` be spanned across the table's columns
  (`TRUE`) or placed in the rows row label (`FALSE`). Defaults to
  `FALSE` currently.

## Value

`tt` pruned based on the arguments, or, if pruning would remove all
rows, a TableTree with the same column structure, and one row containing
the empty message spanning all columns

## Examples

``` r
prfun <- function(tt) TRUE

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STRATA1") |>
  split_rows_by("SEX") |>
  analyze("AGE")
tbl <- build_table(lyt, ex_adsl)

safe_prune_table(tbl, prfun)
#>                               A: Drug X         B: Placebo         C: Combination   
#>                             A     B     C     A      B     C      A       B      C  
#> ————————————————————————————————————————————————————————————————————————————————————
#>  - No Data To Display -                                                             
```
