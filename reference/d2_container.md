# Create a D2 container or map

A container or map is a group of diagram elements. Some special chart
types, such as the SQL Table created by
[`d2_sql_table()`](https://elipousson.github.io/d2r/reference/d2_sql_table.md)
must be set up as a container. Using containers can also help set
element properties including the class, label, shape and dimensions
(width and height) for a set of diagram elements.

## Usage

``` r
d2_container(
  ...,
  lines = NULL,
  id = NULL,
  class = NULL,
  name = NULL,
  label = NULL,
  shape = NULL,
  width = NULL,
  height = NULL,
  icon = NULL,
  style = d2_style(),
  collapse = NULL
)
```

## Arguments

- ...:

  Diagram text.

- lines:

  Optional diagram text. If supplied `lines` is recycled to match the
  length of `id`.

- id:

  D2 diagram map identifier. All other container or map attributes are
  recycled to match the length of `id` using
  [`vctrs::vec_recycle_common()`](https://vctrs.r-lib.org/reference/vec_recycle.html)

- class:

  Diagram class

- name:

  Diagram container name

- label:

  Diagram label

- shape:

  Diagram shape

- width, height:

  Diagram width and height (in pixels?)

- icon:

  Diagram icon

- style:

  A string or results from the
  [`d2_style()`](https://elipousson.github.io/d2r/reference/d2_style.md)
  helper function.

- collapse:

  If `collapse = "\n"`, return a string with D2 diagram code for a
  container. If `collapse = NULL` (default), return a character vector
  with the same length as id.

## Value

A string or character vector with D2 diagram code.

## Examples

``` r
d2_container(
  "team1 <-> team2", "team2 <-> team3", "team3 <-> team1",
  id = c("round1", "round2")
)
#> [1] "round1: {\n        team1 <-> team2\nteam2 <-> team3\nteam3 <-> team1\n}"
#> [2] "round2: {\n        team1 <-> team2\nteam2 <-> team3\nteam3 <-> team1\n}"

d2_container(
  lines = c("shark -> fish: eats", "lion -> gazelle: eats"),
  id = c("ocean", "savannah")
)
#> [1] "ocean: {\n        shark -> fish: eats\n}"     
#> [2] "savannah: {\n        lion -> gazelle: eats\n}"
```
