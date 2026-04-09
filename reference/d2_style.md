# Create a D2 style

Learn more: <https://d2lang.com/tour/style>

## Usage

``` r
d2_style(
  ...,
  inline = FALSE,
  level = c("shape", "connector", "root"),
  after = "\n"
)
```

## Arguments

- ...:

  Named attributes for style. Underscores in parameter names are
  substituted for dashes to match D2 style, e.g. `"stroke_dash"` is
  converted to `"stroke-dash"`.

- inline:

  If `TRUE` or a string value , return a string instead of an array.
  Errors in either case and more than a single parameter is supplied. If
  string, the string is assumed to be the id for a shape, connector, or
  container/map and appended before the "style" keyword.

- level:

  String. "shape", "connector", or "root". If "root", limit style to
  supported attributes for root-level style. See
  <https://d2lang.com/tour/style#root> for more information.

- after:

  String after each line of the style specification. Defaults to `"\n"`

## Examples

``` r
d2_style(stroke = "#f4a261", inline = TRUE)
#> [1] "style.stroke: #f4a261\n"

d2_style(fill = "green", inline = "tree")
#> [1] "tree.style.fill: green\n"

d2_style(animated = TRUE, stroke_dash = 3, level = "connector")
#> [1] "style: {\n    animated: TRUE\n    stroke-dash: 3\n}"
```
