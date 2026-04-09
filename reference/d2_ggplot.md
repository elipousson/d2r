# Plot a D2 diagram as a ggplot2 using `magick::image_ggplot()`

`d2_ggplot()` renders a D2 diagram input character vector or file path
(to a diagram file or rendered diagram) and plots the image with
[`magick::image_ggplot()`](https://docs.ropensci.org/magick/reference/image_ggplot.html).

## Usage

``` r
d2_ggplot(
  x,
  ...,
  density = 150,
  width = NULL,
  height = NULL,
  interpolate = FALSE,
  arg = caller_arg(x),
  call = caller_env()
)
```

## Arguments

- x:

  A character vector of diagram text or a file path for a D2 diagram
  file to render as a diagram with
  [`d2_render()`](https://elipousson.github.io/d2r/reference/d2_render.md)
  before plotting. A file path for a rendered D2 diagram file one using
  a svg, pdf, png, or gif file extension is also allowed.

- ...:

  Additional parameters passed to
  [`d2_diagram()`](https://elipousson.github.io/d2r/reference/d2_diagram.md).

- density:

  Resolution to render pdf passed to
  [`magick::image_read_pdf()`](https://docs.ropensci.org/magick/reference/editing.html).
  Default `150`.

- width:

  in pixels

- height:

  in pixels

- interpolate:

  passed to ggplot2::annotation_raster

- arg:

  An argument name as a string. This argument will be mentioned in error
  messages as the input that is at the origin of a problem.

- call:

  The execution environment of a currently running function, e.g.
  `caller_env()`. The function will be mentioned in error messages as
  the source of the error. See the `call` argument of
  [`abort()`](https://rlang.r-lib.org/reference/abort.html) for more
  information.
