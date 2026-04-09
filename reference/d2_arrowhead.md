# Specify attributes for a single arrowhead or a pair of source and target arrowheads

`d2_arrowhead()` specifies the label, shape, and fill for a source or
target arrowhead. `d2_arrowheads()` allows the specification of both a
source and target arrowhead.

## Usage

``` r
d2_arrowhead(
  shape = "triangle",
  type = c("target", "source"),
  label = NULL,
  filled = NULL
)

d2_arrowheads(..., target = list(), source = list(), id = NULL)
```

## Arguments

- shape:

  See `d2r::keys_d2[["arrowhead"]]` for allowed shape values.

- type:

  Type of arrowhead: "target" or "source".

- label:

  Short arrowhead label.

- filled:

  Must be `NULL` or logical. Ignored if shape is `"arrow"` or a crow's
  foot shape (any shape starting with `"cf-"`).

- ...:

  Named parameters used to set values for both the source and target
  arrowheads.

- target, source:

  Named list of arguments (excluding type) passed to `d2_arrowhead()`.

- id:

  D2 diagram map identifier. All other container or map attributes are
  recycled to match the length of `id` using
  [`vctrs::vec_recycle_common()`](https://vctrs.r-lib.org/reference/vec_recycle.html)

## Examples

``` r
d2_arrowhead(
  shape = "cf-many-required",
  label = "Many required"
)
#> [1] "target-arrowhead: Many required {\nshape: cf-many-required\n}"

example_diagram <- "A -> B"

d2_arrowheads(
  shape = "triangle",
  id = example_diagram,
  filled = FALSE
)
#> [1] "A -> B: {\n        source-arrowhead: {\nstyle.filled: false\n}\ntarget-arrowhead: {\nstyle.filled: false\n}\n}"

d2_arrowheads(
  source = list(
    shape = "triangle",
    label = "source",
    filled = FALSE
  ),
  target = list(
    shape = "circle",
    filled = TRUE
  ),
  id = example_diagram
)
#> [1] "A -> B: {\n        source-arrowhead: source {\nstyle.filled: false\n}\ntarget-arrowhead: {\nshape: circle\nstyle.filled: true\n}\n}"
```
