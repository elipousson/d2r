% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/render.R
\name{d2_render}
\alias{d2_render}
\alias{d2_include}
\title{Render a D2 input file to an output file}
\usage{
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
}
\arguments{
\item{input}{Required input file with \code{"d2"} file extension.}

\item{output}{Output filename.}

\item{...}{Additional input flags. Optional character vector.}

\item{fileext}{One of: svg, pdf, png, gif, pptx. Ignored if \code{output} is
supplied.}

\item{layout}{Layout. One of: elk, dagre, taga}

\item{theme}{Diagram theme name or code. Theme names are not case sensitive.
Use \code{\link[=d2_themes]{d2_themes()}} to list available themes.}

\item{sketch}{If \code{TRUE}, use a hand-drawn style for the rendered diagram.}

\item{pad}{Diagram padding in pixels. Numeric value or string coercible to
whole number.}

\item{animate_interval}{Required if fileext is \code{"gif"} or \code{output} uses a
\code{"gif"} file extension.}

\item{font_family}{String with font family name. Passed to
\code{\link[systemfonts:match_fonts]{systemfonts::match_font()}} to set the regular, bold, and italic fonts.
Fonts must be a TTF file. D2 has a flag for setting semi-bold fonts but
this is not supported at this time. \code{font_family} can also be supplied as a
named vector of font family names with elements named regular, italic, and
bold to use different font family names for the different styles of fonts.
\code{font_family} can also be a vector of TTF file paths to allow users to
provide distinct fonts for regular, italic, and bold text.}

\item{overwrite}{If \code{FALSE} and output exists, abort rendering. Defaults to
\code{TRUE}.}

\item{preview}{If \code{TRUE}, preview the rendered plot using \code{\link[=d2_ggplot]{d2_ggplot()}}.
Reqires the magick package.}

\item{call}{The execution environment of a currently
running function, e.g. \code{caller_env()}. The function will be
mentioned in error messages as the source of the error. See the
\code{call} argument of \code{\link[rlang:abort]{abort()}} for more information.}

\item{auto_pdf}{Whether to use PDF images automatically when the output
format is LaTeX. If \code{TRUE}, then e.g. \file{foo/bar.png} will be
replaced by \file{foo/bar.pdf} if the latter exists. This can be useful
since normally PDF images are of higher quality than raster images like
PNG, when the output is LaTeX/PDF.}

\item{dpi}{DPI (dots per inch) value. Used to calculate the output width (in
inches) of the images. This will be their actual width in pixels, divided
by \code{dpi}. If not provided, the chunk option \code{dpi} is used; if
\code{NA}, the output width will not be calculated.}

\item{rel_path}{Whether to automatically convert absolute paths to relative
paths. If you know for sure that absolute paths work, you may set this
argument or the global option \code{knitr.graphics.rel_path} to
\code{FALSE}.}

\item{error}{Whether to signal an error if any files specified in the
\code{path} argument do not exist and are not web resources.}
}
\description{
Render a D2 input file to an output file

\code{\link[=d2_include]{d2_include()}} combines \code{\link[=d2_render]{d2_render()}} with \code{\link[knitr:include_graphics]{knitr::include_graphics()}}.
}
