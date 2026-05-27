# Create a D2 SQL Table Diagram

Create a D2 SQL Table Diagram

## Usage

``` r
d2_sql_table(
  data = NULL,
  visibility = NULL,
  col_types = vec_ptype_abbr,
  id = NULL,
  label = id,
  ...
)
```

## Arguments

- data:

  A data frame used for column names and types. Ignored if `col_types`
  is a named vector. Required if `col_types` is unnamed or a function.

- visibility:

  One or more of: , +, -, \#. Length is recycled to match the number of
  columns in `data`.

- col_types:

  A named character vector, an unnamed character vector with the same
  length as the number of columns in `data`, or a function that returns
  a character vector based on the columns from `data`. Defaults to
  [vctrs::vec_ptype_abbr](https://vctrs.r-lib.org/reference/vec_ptype_full.html).
  Other recommended options are
  [vctrs::vec_ptype_full](https://vctrs.r-lib.org/reference/vec_ptype_full.html)
  or [base::typeof](https://rdrr.io/r/base/typeof.html). If `types` is a
  named character vector, the column names of `data` are ignored the
  names of types used instead.

- id:

  Diagram container id passed to
  [`d2_container()`](https://elipousson.github.io/d2r/reference/d2_container.md).
  Defaults to `NULL`. If `NULL`, `id` is set to
  `rlang::caller_arg(data)` if `data` is supplied or a random string if
  `data = NULL`.

- label:

  Diagram label. Defaults to `id`. If supplied, `label` is displayed
  instead of `id`.

- ...:

  Arguments passed on to
  [`d2_container`](https://elipousson.github.io/d2r/reference/d2_container.md)

  `lines`

  :   Optional diagram text. If supplied `lines` is recycled to match
      the length of `id`.

  `class`

  :   Diagram class

  `name`

  :   Diagram container name

  `width,height`

  :   Diagram width and height (in pixels?)

  `style`

  :   A string or results from the
      [`d2_style()`](https://elipousson.github.io/d2r/reference/d2_style.md)
      helper function.

  `icon`

  :   Diagram icon

  `collapse`

  :   If `collapse = "\n"`, return a string with D2 diagram code for a
      container. If `collapse = NULL` (default), return a character
      vector with the same length as id.

## Value

A character string with D2 diagram code using the "sql_type" shape.

## Examples

``` r

d2_sql_table(mtcars[, 1:4])
#> [1] "\"mtcars[, 1:4]\": {\n       label: \"mtcars[, 1:4]\"\n     shape: sql_table\n        mpg: dbl\n\n    cyl: dbl\n\n    disp: dbl\n\n    hp: dbl\n\n}"

d2_sql_table(
  label = "col_types Example",
  col_types = c("name" = "char", "num" = "int")
)
#> [1] "sWLkluue: {\n       label: col_types Example\n     shape: sql_table\n        name: char\n\n    num: int\n\n}"
```
