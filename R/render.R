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
#' @param overwrite If `FALSE` and output exists, abort rendering. Defaults to
#'   `TRUE`.
#' @param ... Additional input flags. Optional character vector.
#' @export
d2_render <- function(
    input,
    output = NULL,
    fileext = "svg",
    layout = "elk",
    theme = NULL,
    sketch = NULL,
    pad = NULL,
    animate_interval = NULL,
    ...,
    overwrite = TRUE) {
  check_d2_file(input)

  fileext <- arg_match(fileext, keys_d2[["fileext"]])

  if (!is.null(layout)) {
    layout <- arg_match(layout, keys_d2[["layout"]])
  }

  if (!is.null(theme)) {
    theme <- match_d2_theme(theme)
  }

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

  params <- vctrs::list_drop_empty(params)

  templates <- list(
    theme = "--theme={theme}",
    layout = "--layout={layout}",
    sketch = "--sketch={tolower(sketch)}",
    pad = "--pad={as.numeric(pad)}",
    pad = "--animate-interval={as.numeric(animate_interval)}",
    input = "{input}",
    output = "{output}"
  )

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

  return(invisible(output))
}
