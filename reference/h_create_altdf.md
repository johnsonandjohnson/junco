# Create Alternative Data Frame

Creates an alternative data frame based on the current split context.

## Usage

``` r
h_create_altdf(
  .spl_context,
  .df_row,
  denomdf,
  denom_by = NULL,
  id,
  variables,
  denom
)
```

## Arguments

- .spl_context:

  Current split context.

- .df_row:

  Current data frame row.

- denomdf:

  Denominator data frame.

- denom_by:

  Denominator grouping variable.

- id:

  Identifier variable.

- variables:

  Variables to include in the analysis.

- denom:

  Denominator type.

## Value

Grand parent dataset.
