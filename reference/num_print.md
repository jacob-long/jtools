# Numbering printing with signed zeroes and trailing zeroes

This function will print exactly the amount of digits requested as well
as signed zeroes when appropriate (e.g, `-0.00`).

## Usage

``` r
num_print(x, digits = getOption("jtools-digits", 2), format = "f")
```

## Arguments

- x:

  The number(s) to print

- digits:

  Number of digits past the decimal to print

- format:

  equal to `"d"` (for integers), `"f"`, `"e"`, `"E"`, `"g"`, `"G"`,
  `"fg"` (for reals). Default is `"f"`
