themes_d2 <- c(
  "Neutral default", "Neutral Grey", "Flagship Terrastruct", "Cool classics",
  "Mixed berry blue", "Grape soda", "Aubergine", "Colorblind clear",
  "Vanilla nitro cola", "Orange creamsicle", "Shirley temple", "Earth tones",
  "Everglade green", "Buttered toast", "Terminal", "Terminal Grayscale",
  "Origami", "Dark Mauve", "Dark Flagship Terrastruct"
)

## code to prepare `d2` dataset goes here
themes_d2 <- set_names(
  c(
    0, 1, 3, 4, 5, 6, 7, 8,
    100, 101, 102, 103, 104, 105,
    300, 301, 302,
    200, 201
  ),
  themes_d2
)

usethis::use_data(themes_d2, overwrite = TRUE)

keys_d2 <- list(

  # https://d2lang.com/tour/layouts#layout-engines
  "layout" = c("elk", "dagre", "taga"),

  # https://d2lang.com/tour/themes
  "theme" = set_names(themes_d2, tolower(names(themes_d2))),

  # https://d2lang.com/tour/exports
  "fileext" = c("svg", "pdf", "png", "gif", "pptx"),

  # https://d2lang.com/tour/connections
  "connector" = c("--", "->", "<-", "<->"),

  # https://d2lang.com/tour/connections#arrowheads
  "arrowhead" = c(
    "triangle", "arrow", "diamond", "circle",
    "cf-one", "cf-one-required", "cf-many", "cf-many-required"
  ),

  # https://d2lang.com/tour/layouts#direction
  "direction" = c("up", "down", "right", "left"),

  # https://d2lang.com/tour/style
  "style" = c(
    "opacity", "stroke", "fill", "fill-pattern", "stroke-width",
    "stroke-dash", "border-radius", "shadow", "3D", "multiple",
    "double-border", "font", "font-size", "font-color", "animated",
    "bold", "italic", "underline", "text-transform", "root"
  ),

  # https://d2lang.com/tour/uml-classes#visibilities
  # https://www.uml-diagrams.org/visibility.html
  "visibility" = set_names(
    c("", "+", "-", "#"),
    c("public", "public", "private", "protected")
  ),

  # https://d2lang.com/tour/sql-tables
  "constraints" = c(
    "primary_key", "PK",
    "foreign_key", "FK",
    "unique", "UNQ"
  ),

  # https://d2lang.com/tour/grid-diagrams
  "grid" = c(
    "grid-rows",
    "grid-columns"
  )
)

usethis::use_data(keys_d2, overwrite = TRUE)
