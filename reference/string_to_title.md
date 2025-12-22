# Title Case Conversion

Title Case Conversion

## Usage

``` r
string_to_title(x)
```

## Arguments

- x:

  (`character` or `factor`)  
  Input string

## Value

x converted to title case (first letter of each word capitalized)

## Examples

``` r
x <- c("THIS IS an eXaMple", "statement TO CAPItaliZe")
string_to_title(x)
#> [1] "This Is An Example"      "Statement To Capitalize"

x <- factor(
  c("OPTIMAL DOSE", "UNDERDOSE"),
  levels = c("OPTIMAL DOSE", "UNDERDOSE", "OVERDOSE")
)
string_to_title(x)
#> [1] Optimal Dose Underdose   
#> Levels: Optimal Dose Underdose Overdose
```
