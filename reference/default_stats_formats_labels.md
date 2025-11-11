# Get default statistical methods and their associated formats, labels, and indent modifiers

**\[experimental\]**

Utility functions to get valid statistic methods for different method
groups (`.stats`) and their associated formats (`.formats`), labels
(`.labels`), and indent modifiers (`.indent_mods`). This utility is used
across `junco`, but some of its working principles can be seen in
[`tern::analyze_vars()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html).
See notes to understand why this is experimental.

## Usage

``` r
junco_get_stats(
  method_groups = "analyze_vars_numeric",
  stats_in = NULL,
  custom_stats_in = NULL,
  add_pval = FALSE
)

junco_get_formats_from_stats(stats, formats_in = NULL, levels_per_stats = NULL)

junco_get_labels_from_stats(
  stats,
  labels_in = NULL,
  levels_per_stats = NULL,
  label_attr_from_stats = NULL
)

get_label_attr_from_stats(x_stats)

junco_get_indents_from_stats(stats, indents_in = NULL, levels_per_stats = NULL)

format_stats(
  x_stats,
  method_groups,
  stats_in,
  formats_in,
  labels_in,
  indents_in
)

junco_default_stats

junco_default_formats

junco_default_labels

junco_default_indents
```

## Format

- `junco_default_stats` is a named list of available statistics, with
  each element named for their corresponding statistical method group.

&nbsp;

- `junco_default_formats` is a named vector of available default
  formats, with each element named for their corresponding statistic.

&nbsp;

- `junco_default_labels` is a named `character` vector of available
  default labels, with each element named for their corresponding
  statistic.

&nbsp;

- `junco_default_indents` is a named `integer` vector of available
  default indents, with each element named for their corresponding
  statistic. Only indentations different from zero need to be recorded
  here.

## Arguments

- method_groups:

  (`character`)  
  indicates the statistical method group (`junco` analyze function) to
  retrieve default statistics for. A character vector can be used to
  specify more than one statistical method group.

- stats_in:

  (`character`)  
  statistics to retrieve for the selected method group. If custom
  statistical functions are used, `stats_in` needs to have them in too.

- custom_stats_in:

  (`character`)  
  custom statistics to add to the default statistics.

- add_pval:

  (`flag`)  
  should `'pval'` (or `'pval_counts'` if `method_groups` contains
  `'analyze_vars_counts'`) be added to the statistical methods?

- stats:

  (`character`)  
  statistical methods to return defaults for.

- formats_in:

  (named `vector`)  
  custom formats to use instead of defaults. Can be a character vector
  with values from
  [`formatters::list_valid_format_labels()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
  or custom format functions. Defaults to `NULL` for any rows with no
  value is provided.

- levels_per_stats:

  (named `list` of `character` or `NULL`)  
  named list where the name of each element is a statistic from `stats`
  and each element is the levels of a `factor` or `character` variable
  (or variable name), each corresponding to a single row, for which the
  named statistic should be calculated for. If a statistic is only
  calculated once (one row), the element can be either `NULL` or the
  name of the statistic. Each list element will be flattened such that
  the names of the list elements returned by the function have the
  format `statistic.level` (or just `statistic` for statistics
  calculated for a single row). Defaults to `NULL`.

- labels_in:

  (named `character`)  
  custom labels to use instead of defaults. If no value is provided, the
  variable level (if rows correspond to levels of a variable) or
  statistic name will be used as label.

- label_attr_from_stats:

  (named `list`)  
  if `labels_in = NULL`, then this will be used instead. It is a list of
  values defined in statistical functions as default labels. Values are
  ignored if `labels_in` is provided or `''` values are provided.

- x_stats:

  (`list`)  
  with the statistics results.

- indents_in:

  (named `integer`)  
  custom row indent modifiers to use instead of defaults. Defaults to
  `0L` for all values.

## Value

- `junco_get_stats()` returns a `character` vector of statistical
  methods.

&nbsp;

- `junco_get_formats_from_stats()` returns a named list of formats as
  strings or functions.

&nbsp;

- `junco_get_labels_from_stats()` returns a named list of labels as
  strings.

&nbsp;

- `junco_get_indents_from_stats()` returns a named list of indentation
  modifiers as integers. By default all of the indentations will be
  zero.

&nbsp;

- `format_stats()` returns the correspondingly formatted
  [`rtables::in_rows()`](https://insightsengineering.github.io/rtables/latest-tag/reference/in_rows.html)
  result.

## Details

Current choices for `type` are `counts` and `numeric` for
[`tern::analyze_vars()`](https://insightsengineering.github.io/tern/latest-tag/reference/analyze_variables.html)
and affect `junco_get_stats()`.

## Functions

- `junco_get_stats()`: Get statistics available for a given method group
  (analyze function). To check available defaults see
  `junco_default_stats` list.

- `junco_get_formats_from_stats()`: Get formats corresponding to a list
  of statistics. To check available defaults see list
  `junco_default_formats`.

- `junco_get_labels_from_stats()`: Get labels corresponding to a list of
  statistics. To check for available defaults see list
  `junco_default_labels`.

- `get_label_attr_from_stats()`: Get label attributes from statistics
  list.

- `junco_get_indents_from_stats()`: Get row indent modifiers
  corresponding to a list of statistics/rows.

- `format_stats()`: Format statistics results according to format
  specifications.

- `junco_default_stats`: Named list of available statistics by method
  group for `junco`.

- `junco_default_formats`: Named vector of default formats for `junco`.

- `junco_default_labels`: Named `character` vector of default labels for
  `junco`.

- `junco_default_indents`: Named `integer` vector of default indents for
  `junco`.

## Note

These defaults are experimental because we use the names of functions to
retrieve the default statistics. This should be generalized in groups of
methods according to more reasonable groupings.

These functions have been modified from the `tern` file
`utils_default_stats_formats_labels.R`. This file contains `junco`
specific wrappers of functions called within the `afun` functions, in
order to point to `junco` specific default statistics, formats and
labels.

Formats in `tern` or `junco` and `rtables` can be functions that take in
the table cell value and return a string. This is well documented in
[`vignette('custom_appearance', package = 'rtables')`](https://insightsengineering.github.io/rtables/latest-tag/articles/custom_appearance.html).
