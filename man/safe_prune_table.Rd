% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pruning_functions.R
\name{safe_prune_table}
\alias{safe_prune_table}
\title{Safely Prune Table With Empty Table Message If Needed}
\usage{
safe_prune_table(
  tt,
  prune_func = prune_empty_level,
  stop_depth = NA,
  empty_msg = " - No Data To Display - ",
  spancols = FALSE
)
}
\arguments{
\item{tt}{(\code{TableTree} or related class)\cr a \code{TableTree} object representing a populated table.}

\item{prune_func}{(\code{function})\cr a function to be called on each subtree which returns \code{TRUE} if the
entire subtree should be removed.}

\item{stop_depth}{(\code{numeric(1)})\cr the depth after which subtrees should not be checked for pruning.
Defaults to \code{NA} which indicates pruning should happen at all levels.}

\item{empty_msg}{character(1). The message to place in the table
if no rows were left after pruning}

\item{spancols}{logical(1). Should \code{empty_msg} be spanned
across the table's columns (\code{TRUE}) or placed in the
rows row label (\code{FALSE}). Defaults to \code{FALSE} currently.}
}
\value{
\code{tt} pruned based on the arguments, or, if
pruning would remove all rows, a TableTree with the
same column structure, and one row containing the
empty message spanning all columns
}
\description{
Safely Prune Table With Empty Table Message If Needed
}
\examples{
prfun <- function(tt) TRUE

lyt <- basic_table() |>
  split_cols_by("ARM") |>
  split_cols_by("STRATA1") |>
  split_rows_by("SEX") |>
  analyze("AGE")
tbl <- build_table(lyt, ex_adsl)

safe_prune_table(tbl, prfun)
}
