# Render a D2 input file to an output file

Render a D2 input file to an output file

`d2_include()` combines `d2_render()` with
[`knitr::include_graphics()`](https://rdrr.io/pkg/knitr/man/include_graphics.html).

## Usage

``` r
d2_render(
  input,
  output = NULL,
  ...,
  fileext = "svg",
  layout = getOption("d2r.layout", "elk"),
  theme = getOption("d2r.theme"),
  sketch = getOption("d2r.sketch"),
  pad = getOption("d2r.pad"),
  animate_interval = NULL,
  font_family = NULL,
  overwrite = TRUE,
  preview = FALSE,
  call = caller_env()
)

d2_include(
  input,
  output = NULL,
  ...,
  auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE),
  dpi = NULL,
  rel_path = getOption("knitr.graphics.rel_path", TRUE),
  error = getOption("knitr.graphics.error", TRUE)
)
```

## Arguments

- input:

  Required input file with `"d2"` file extension.

- output:

  Output filename.

- ...:

  Additional input flags. Optional character vector.

- fileext:

  One of: svg, pdf, png, gif, pptx, txt. Ignored if `output` is
  supplied.

- layout:

  Layout. One of: elk, dagre, taga

- theme:

  Diagram theme name or code. Theme names are not case sensitive. Use
  [`d2_themes()`](https://elipousson.github.io/d2r/reference/d2-utils.md)
  to list available themes.

- sketch:

  If `TRUE`, use a hand-drawn style for the rendered diagram.

- pad:

  Diagram padding in pixels. Numeric value or string coercible to whole
  number.

- animate_interval:

  Required if fileext is `"gif"` or `output` uses a `"gif"` file
  extension.

- font_family:

  String with font family name. Passed to
  [`systemfonts::match_font()`](https://systemfonts.r-lib.org/reference/match_fonts.html)
  to set the regular, bold, and italic fonts. Fonts must be a TTF file.
  D2 has a flag for setting semi-bold fonts but this is not supported at
  this time. `font_family` can also be supplied as a named vector of
  font family names with elements named regular, italic, and bold to use
  different font family names for the different styles of fonts.
  `font_family` can also be a vector of TTF file paths to allow users to
  provide distinct fonts for regular, italic, and bold text.

- overwrite:

  If `FALSE` and output exists, abort rendering. Defaults to `TRUE`.

- preview:

  If `TRUE`, preview the rendered plot using
  [`d2_ggplot()`](https://elipousson.github.io/d2r/reference/d2_ggplot.md).
  Reqires the magick package.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.

- auto_pdf:

  Whether to use PDF images automatically when the output format is
  LaTeX. If `TRUE`, then e.g. `foo/bar.png` will be replaced by
  `foo/bar.pdf` if the latter exists. This can be useful since normally
  PDF images are of higher quality than raster images like PNG, when the
  output is LaTeX/PDF.

- dpi:

  DPI (dots per inch) value. Used to calculate the output width (in
  inches) of the images. This will be their actual width in pixels,
  divided by `dpi`. If not provided, the chunk option `dpi` is used; if
  `NA`, the output width will not be calculated.

- rel_path:

  Whether to automatically convert absolute paths to relative paths. If
  you know for sure that absolute paths work, you may set this argument
  or the global option `knitr.graphics.rel_path` to `FALSE`.

- error:

  Whether to signal an error if any files specified in the `path`
  argument do not exist and are not web resources.
