# Get default statistical methods and their associated formats, labels, and indent modifiers

**\[experimental\]**

## Usage

``` r
tern_get_stats(
  method_groups = "analyze_vars_numeric",
  stats_in = NULL,
  custom_stats_in = NULL,
  add_pval = FALSE,
  tern_defaults = tern_default_stats
)

tern_get_formats_from_stats(
  stats,
  formats_in = NULL,
  levels_per_stats = NULL,
  tern_defaults = tern_default_formats
)

tern_get_labels_from_stats(
  stats,
  labels_in = NULL,
  levels_per_stats = NULL,
  label_attr_from_stats = NULL,
  tern_defaults = tern_default_labels
)

tern_get_indents_from_stats(
  stats,
  indents_in = NULL,
  levels_per_stats = NULL,
  tern_defaults = stats::setNames(as.list(rep(0L, length(stats))), stats)
)

tern_default_labels
```

## Format

An object of class `character` of length 40.

## Functions

- `tern_get_stats()`: Get statistics available for a given method group
  (analyze function).

- `tern_get_formats_from_stats()`: Get formats corresponding to a list
  of statistics.

- `tern_get_labels_from_stats()`: Get labels corresponding to a list of
  statistics.

- `tern_get_indents_from_stats()`: Get row indent modifiers
  corresponding to a list of statistics/rows.

- `tern_default_labels`: Named `character` vector of default labels for
  `tern`. This is only copied here from the latest GitHub version,
  because otherwise a tern test fails.

## Note

These functions have been copied from the `tern` package file
`utils_default_stats_formats_labels.R` from GitHub development version
0.9.7.9017. Slight modifications have been applied to enhance
functionality:

- `tern_get_stats` added the `tern_stats` argument to avoid hardcoding
  within the function's body.

- `tern_get_labels_from_stats` is more careful when receiving partial
  `labels_in` and partial `label_attr_from_stats`.

Once these features are included in the `tern` package, this file could
be removed from the `junco` package, and the functions could be used
from the `tern` namespace directly.
