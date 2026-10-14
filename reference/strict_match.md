# Strictly Match a Value in a Character Vector

Finds a unique match of a value in either the odd or even positions of a
character vector. An error is raised if no match or more than one match
is found in the selected positions.

## Usage

``` r
strict_match(x, y, odd = TRUE)
```

## Arguments

- x:

  (`character(1)`)  
  The value to match.

- y:

  (`character`)  
  The character vector in which to search for `x`.

- odd:

  (`flag`)  
  Whether to restrict the match to odd positions. Defaults to `TRUE`. If
  `FALSE`, only even positions are considered.

## Value

An integer containing the unique position of `x` in `y`.

## Author

WW

## Examples

``` r
if (FALSE) { # \dontrun{
strict_match("A", c("A", "Placebo"))

strict_match("SEX", c("SomeVar", "SomeVal", "SEX", "Male"))

strict_match("ARM", c("SEX", "Male"))
strict_match("Male", c("SEX", "Male"))
strict_match("ARM", c("ARM", "Placebo", "ARM", "Active"))
} # }
```
