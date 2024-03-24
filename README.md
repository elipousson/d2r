
<!-- README.md is generated from README.Rmd. Please edit that file -->

# d2r

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of d2r is to support the creation, reading, writing, and
rendering of D2 diagrams using R.

What is D2? From the [D2 Introduction](https://d2lang.com/tour/intro):

> D2 is a diagram scripting language that turns text to diagrams. It
> stands for Declarative Diagramming. Declarative, as in, you describe
> what you want diagrammed, it generates the image.

I am a contributor to the [D2 Extension for
Quarto](https://github.com/data-intuitive/quarto-d2) and created this
package to experiment with the possibilities of native R diagram
creation and rendering.

Note that this package is experimental and the process for building and
editing D2 diagrams is likely to change. I also hope to add a knitr
engine to support d2 code blocks within a RMarkdown or Quarto document.

## Installation

You can install the development version of d2r like so:

``` r
# pak::pkg_install("elipousson/d2r")
```

## Example

``` r
library(d2r)
## basic example code
```

If d2 is installed and available on your PATH, you can check the
version:

``` r
d2_version()
#> ℹ v0.6.3 <https://d2lang.com/releases/0.6.3>
```

If you have D2 installed with Homebrew, you may have trouble rendering a
diagram within a RMarkdown. If so, you may need to set an environmental
variable that points to the D2 PATH.

Use `d2_which()` to locate the path for the installed version of D2:

``` r
d2_which()
#> [1] "/opt/homebrew/bin/d2"
```

This [2018 blog post by Tony
Tsai](https://blog.tonytsai.name/blog/2018-05-07-setting-path-variable-for-gs-command-in-rstudio/)
on trouble-shooting `PATH` variable issues for `brew`-installed
commands.

Here is an example of basic diagram creation:

``` r
d2_diagram(c("a" = "b"))
#>        a 
#> "a -> b"
```

Example of diagram with specified connectors:

``` r
d2_diagram(
  c("x" = "y", "y" = "z"),
  connector = c("->", "<-")
)
#>        x        y 
#> "x -> y" "y <- z"
```

Example of rendered diagram:

``` r
diagram <- d2_diagram(c("a" = "b", "x" = "y"))  

file <- tempfile(fileext = ".d2")

output <- paste0(knitr::opts_chunk$get("fig.path"), "example.png")

d2_write(diagram, file)

d2_render(file, output, theme = "everglade green")

knitr::include_graphics(output)
```

<img src="man/figures/README-example.png" width="100%" />
