# Title Case Conversion

**\[stable\]**

Convert strings to title case (first letter of each word capitalized),
with the option to keep specified words, such as conjunctions,
lowercase.

## Usage

``` r
string_to_title(x, lowercase_words = NULL)
```

## Arguments

- x:

  (`character` or `factor`)  
  Input string.

- lowercase_words:

  (`character` or `NULL`)  
  Words that should remain lowercase when converting to title case.
  Matching is case-insensitive. Specified words are only converted to
  lowercase when they are not the first word in the string. The first
  word is always capitalized, even if it is included in
  `lowercase_words`.

## Value

`x` converted to title case, with words specified in `lowercase_words`
kept lowercase.

## Examples

``` r
x <- c("THIS IS an eXaMple", "statement TO CAPItaliZe")
string_to_title(x)
#> [1] "This Is An Example"      "Statement To Capitalize"
string_to_title(x, c("is", "an", "to"))
#> [1] "This is an Example"      "Statement to Capitalize"

x <- factor(
  c("OPTIMAL DOSE", "UNDERDOSE"),
  levels = c("OPTIMAL DOSE", "UNDERDOSE", "OVERDOSE")
)
string_to_title(x)
#> [1] Optimal Dose Underdose   
#> Levels: Optimal Dose Underdose Overdose
```
