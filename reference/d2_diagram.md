# Create a D2 diagram from a named vector and connectors

Note: this is an initial experimental implementation of diagram building
and may change or be deprecated in the future.

## Usage

``` r
d2_diagram(
  lines = NULL,
  ...,
  connector = NULL,
  id = NULL,
  direction = getOption("d2r.direction"),
  import = NULL,
  collapse = NULL
)
```

## Arguments

- lines:

  A list or character vector. Named elements are connected using the
  supplied `connector` value. Unnamed elements are kept as is and
  assumed to be valid D2 code.

- ...:

  Additional elements for diagram included after initial lines.

- connector:

  Default to `NULL` which uses `"->"` to connect the name of any shape
  to the value of any shape. Must be vector of: –, -\>, \<-, and \<-\>.
  Vector is recycled to match length of named shapes.

- id:

  Identifier for map container for diagram.

- direction:

  Optional direction for diagram. Must be one of: up, down, right, and
  left.

- import:

  File name or names to append as an import to the top of the diagram.
  If named, the standard import method using the name of the import as
  the name of the imported element. If import is unnamed, the partial
  import method is used: https://d2lang.com/tour/imports#partial-imports
  See: <https://d2lang.com/tour/imports>

- collapse:

  an optional character string to separate the results. Not
  [`NA_character_`](https://rdrr.io/r/base/NA.html). When `collapse` is
  a string, the result is always a string
  ([`character`](https://rdrr.io/r/base/character.html) of length 1).

## Examples

``` r
d2_diagram(c("x" = "y"))
#> [1] "x -> y"

d2_diagram(c("a" = "b"), connector = "<-", direction = "up")
#> [1] "direction: up" ""              "a <- b"       

d2_diagram(c("start" = "end"), connector = "<->", import = "imported.d2")
#> [1] "...@imported.d2" ""                "start <-> end"  
```
