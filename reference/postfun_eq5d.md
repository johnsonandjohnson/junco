# Post-processing split function for EQ-5D style column statistics

This helper is designed to be used in the `post` argument of
[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html)
to expand a column facet (e.g. AVAL/BASE/CHG) into the specific
statistics to be analyzed for each subfacet. It returns a split result
instructing rtables which values/labels/subsets to create.

## Usage

``` r
postfun_eq5d(ret, spl, fulldf, .spl_context)
```

## Arguments

- ret:

  ignored; placeholder to match the signature expected by
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html).

- spl:

  ignored; placeholder to match the signature expected by
  [`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html).

- fulldf:

  (`data.frame`) full data used for the split; passed through to
  [`rtables::make_split_result()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_result.html).

- .spl_context:

  split context environment provided by rtables; used here to determine
  the current column level.

## Value

A result from
[`rtables::make_split_result()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_result.html)
selecting EQ-5D style statistics for the current column level.

## Details

Typical usage is to construct a split function like:

- `mysplitfun <- rtables::make_split_fun(post = list(junco::postfun_eq5d))`

Then use `mysplitfun` in your table layout where you split columns by a
variable whose levels are one of "AVAL", "BASE", or "CHG" and want to
analyze different statistics for each.

## See also

[`rtables::make_split_fun()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_fun.html),
[`rtables::make_split_result()`](https://insightsengineering.github.io/rtables/latest-tag/reference/make_split_result.html).
