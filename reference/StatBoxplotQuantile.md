# Boxplot statistics with configurable quantile type

`StatBoxplotQuantile` computes boxplot statistics using a configurable
quantile definition. By default, quartiles use `quantile(type = 2)` to
follow `PCTLDEF = 5` (SAS default). It computes quartiles, whiskers, and
outliers.

## Usage

``` r
StatBoxplotQuantile
```

## Format

An object of class `StatBoxplotQuantile` (inherits from `Stat`,
`ggproto`, `gg`) of length 4.

## Details

Required aesthetics: `x`, `y`.

Computed variables (per group):

- `ymin`: lower whisker (min value within lower fence)

- `lower`: first quartile (Q1)

- `middle`: median (Q2)

- `upper`: third quartile (Q3)

- `ymax`: upper whisker (max value within upper fence)

- `outliers`: list-column of values outside whiskers

- `x`: x position (first value in group)

- `width`: box width used by `GeomBoxplot`

## Quantile details

By default, quartiles are computed with `quantile(type = 2)` to follow
`PCTLDEF = 5` (SAS default). You can change this via the `quantile_type`
argument. Fences are defined as `Q1 - coef * IQR` and `Q3 + coef * IQR`
where `IQR = Q3 - Q1` and `coef` defaults to `1.5`.
