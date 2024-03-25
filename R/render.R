#' @noRd
match_d2_theme <- function(theme,
                           error_call = caller_env()) {
  theme_int <- suppressWarnings(as.integer(theme))

  if (is.character(theme) && is.na(theme_int)) {
    theme_nm <- tolower(theme)

    if (!all(theme_nm %in% tolower(names(themes_d2)))) {
      theme <- arg_match(
        theme,
        values = names(themes_d2),
        error_call = error_call
      )
    }

    return(keys_d2[["theme"]][theme_nm])
  }

  if (is.integer(theme_int) && !any(themes_d2 == theme_int)) {
    cli_abort(
      "{.arg theme} must be a valid theme name
      or one of the following whole numbers: {themes_d2}, not {theme}",
      call = error_call
    )
  }

  theme
}

#' Render a D2 input file to an output file
#'
#' @param input Required input file with `"d2"` file extension.
#' @param output Output filename.
#' @param fileext One of: `r keys_d2[["fileext"]]`. Ignored if `output` is supplied.
#' @param theme Diagram theme name or code. Theme names are not case sensitive.
#'   Use [d2_themes()] to list available themes.
#' @param layout Layout. One of: `r keys_d2[["layout"]]`
#' @param sketch If `TRUE`, use a hand-drawn style for the rendered diagram.
#' @param pad Diagram padding in pixels. Numeric value or string coercible to
#'   whole number.
#' @param animate_interval Required if fileext is `"gif"` or `output` uses a
#'   `"gif"` file extension.
#' @param font_family String with font family name. Passed to
#'   [systemfonts::match_font()] to set the regular, bold, and italic fonts.
#'   Fonts must be a TTF file. D2 has a flag for setting semi-bold fonts but
#'   this is not supported at this time. `font_family` can also be provided as a
#'   named vector of font family names with elements named regular, italic, and
#'   bold to use different font family names for the different styles of fonts.
#' @param overwrite If `FALSE` and output exists, abort rendering. Defaults to
#'   `TRUE`.
#' @param ... Additional input flags. Optional character vector.
#' @export
d2_render <- function(
    input,
    output = NULL,
    fileext = "svg",
    layout = getOption("d2r.layout", "elk"),
    theme = getOption("d2r.theme"),
    sketch = getOption("d2r.sketch"),
    pad = getOption("d2r.pad"),
    animate_interval = NULL,
    font_family = NULL,
    ...,
    overwrite = TRUE) {
  if (is.character(input) && !all(is_d2_file(input))) {
    input <- d2_write(input)
  }

  if (!is.null(layout)) {
    layout <- arg_match(layout, keys_d2[["layout"]])
  }

  if (!is.null(theme)) {
    theme <- match_d2_theme(theme)
  }

  check_logical(sketch, allow_null = TRUE)

  check_number_whole(pad, allow_null = TRUE)

  check_number_whole(animate_interval, allow_null = TRUE)

  check_string(input, allow_null = TRUE)

  fileext <- arg_match(fileext, keys_d2[["fileext"]])

  output <- output %||%
    sub("\\.d2", paste0(".", fileext), basename(input))

  if (!overwrite && file.exists(output)) {
    cli_abort(
      "{.arg output} can't be replaced when {.code overwrite = FALSE}."
    )
  }

  params <- list(
    theme = theme,
    layout = layout,
    sketch = sketch,
    pad = pad,
    animate_interval = animate_interval,
    input = input,
    output = output
  )

  templates <- list(
    theme = "--theme={theme}",
    layout = "--layout={layout}",
    sketch = "--sketch={tolower(sketch)}",
    pad = "--pad={pad}",
    animate_interval = "--animate-interval={animate_interval}",
    input = "{input}",
    output = "{output}"
  )

  if (!is.null(font_family)) {
    templates <- c(
      templates,
      list(
        font = "--font-regular={font}",
        italic = "--font-italic={italic}",
        bold = "--font-bold={bold}"
      )
    )

    font_family_italic <- font_family
    font_family_bold <- font_family

    if (is_named(font_family) && has_length(font_family, 3)) {
      font_family_italic <- font_family[["italic"]] %||% font_family[["regular"]]
      font_family_bold <- font_family[["bold"]] %||% font_family[["regular"]]
      font_family <- font_family[["regular"]]
    }

    font_params <- list(
      font = match_font(font_family)[["path"]],
      italic = match_font(font_family_italic, italic = TRUE)[["path"]],
      bold = match_font(font_family_bold, bold = TRUE)[["path"]]
    )

    if (!all(grepl("\\.ttf$", font_params))) {
      cli_abort(
        "{.arg font_family} must be an installed TTF font."
      )
    }

    params <- c(
      params,
      font_params
    )
  }

  params <- list_drop_empty(params)

  glue_envir <- new_environment(
    params,
    parent = current_env()
  )

  templates <- vec_slice(
    templates,
    i = vec_in(names(templates), names(params))
  )

  args <- vapply(
    templates,
    glue::glue,
    character(1),
    .envir = glue_envir
  )

  out <- exec_d2(args = c(args, ...))

  invisible(output)
}

#' [d2_include()] combines [d2_render()] with [knitr::include_graphics()].
#'
#' @rdname d2_render
#' @inheritParams knitr::include_graphics
#' @export
d2_include <- function(
    input,
    output = NULL,
    ...,
    auto_pdf = getOption("knitr.graphics.auto_pdf", FALSE),
    dpi = NULL,
    rel_path = getOption("knitr.graphics.rel_path", TRUE),
    error = getOption("knitr.graphics.error", TRUE)) {
  output <- d2_render(input = input, output = output, ...)

  knitr::include_graphics(
    output,
    auto_pdf = auto_pdf,
    dpi = dpi,
    rel_path = rel_path,
    error = error
  )
}
