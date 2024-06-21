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
#'   this is not supported at this time. `font_family` can also be supplied as a
#'   named vector of font family names with elements named regular, italic, and
#'   bold to use different font family names for the different styles of fonts.
#'   `font_family` can also be a vector of TTF file paths to allow users to
#'   provide distinct fonts for regular, italic, and bold text.
#' @param overwrite If `FALSE` and output exists, abort rendering. Defaults to
#'   `TRUE`.
#' @param ... Additional input flags. Optional character vector.
#' @export
d2_render <- function(
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
    overwrite = TRUE) {
  # Write input to disk if needed
  if (is.character(input) && !all(is_d2_file(input))) {
    input <- d2_write(input)
  }

  # Check input path
  # FIXME: Add check for file path and file existence
  check_string(input, allow_null = TRUE)

  # Set output path and check for file overwrite
  fileext <- arg_match(fileext, keys_d2[["fileext"]])

  # FIXME: Add knitr context sensitive default for file extension
  output <- output %||%
    sub("\\.d2", paste0(".", fileext), basename(input))

  if (!overwrite && file.exists(output)) {
    cli_abort(
      "{.arg output} can't be replaced when {.code overwrite = FALSE}."
    )
  }

  if (!is.null(theme)) {
    theme <- match_d2_theme(theme)
  }

  # Check remaining D2 render arguments
  check_d2_render_args(
    layout = layout,
    sketch = sketch,
    pad = pad,
    animate_interval = animate_interval
  )

  # Create parameter list
  params <- list(
    theme = theme,
    layout = layout,
    sketch = sketch,
    pad = pad,
    animate_interval = animate_interval,
    font_family = font_family,
    input = input,
    output = output
  )

  exec_d2_args(params = params, ...)
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

#' @noRd
d2_render_elk <- function(
    input,
    output = NULL,
    ...,
    algorithm = "layered",
    nodeNodeBetweenLayers = 70,
    padding = list(
      top = 50,
      left = 50,
      bottom = 50,
      right = 50
    ),
    edgeNodeBetweenLayers = 40,
    nodeSelfLoop = 50) {

  check_string(algorithm, allow_null = TRUE)

  obj_check_vector_named(
    padding,
    nm = c("top", "left", "bottom", "right")
  )

  padding <- paste0(
    "[", paste0(
      paste0(names(padding), "=", padding),
      collapse = ","
    ), "]",
    collapse = ""
  )

  check_number_whole(nodeNodeBetweenLayers, allow_null = TRUE)
  check_number_whole(edgeNodeBetweenLayers, allow_null = TRUE)
  check_number_whole(nodeSelfLoop, allow_null = TRUE)

  elk_params <- list(
    algorithm = algorithm,
    nodeNodeBetweenLayers = nodeNodeBetweenLayers,
    padding = padding,
    edgeNodeBetweenLayers = edgeNodeBetweenLayers,
    nodeSelfLoop = nodeSelfLoop
  )

  elk_templates <- list(
    padding = "--elk-padding={padding}",
    nodeNodeBetweenLayers = "--elk-nodeNodeBetweenLayers={nodeNodeBetweenLayers}",
    edgeNodeBetweenLayers = "--elk-edgeNodeBetweenLayers={edgeNodeBetweenLayers}",
    nodeSelfLoop = "--elk-nodeSelfLoop={nodeSelfLoop}"
  )

  elk_args <- glue_params(
    params = elk_params,
    templates = elk_templates
  )

  d2_render(
    input = input,
    output = output,
    layout = "elk",
    ...,
    as.character(elk_args)
  )
}

#' Match D2 theme to supported options
#'
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

#' Check input arguments for rendering D2 diagram (except theme and font_family)
#' @noRd
check_d2_render_args <- function(
    layout = getOption("d2r.layout", "elk"),
    sketch = getOption("d2r.sketch"),
    pad = getOption("d2r.pad"),
    animate_interval = NULL,
    allow_null = TRUE,
    call = caller_env()) {
  if (!is.null(layout) || (allow_null && is.null(layout))) {
    arg_match(layout, keys_d2[["layout"]], error_call = call)
  }

  check_logical(sketch, allow_null = allow_null, call = call)

  check_number_whole(pad, allow_null = allow_null, call = call)

  check_number_whole(animate_interval, allow_null = allow_null, call = call)
}

#' Append vector of font parameters to supplied parameters
#' @noRd
set_font_family_params <- function(params = NULL,
                                   call = caller_env()) {
  if (is.null(params[["font_family"]])) {
    return(params)
  }

  font_family <- params[["font_family"]]
  params[["font_family"]] <- NULL

  font_params <- list(
    regular = font_family,
    italic = font_family,
    bold = font_family
  )

  if (is_named(font_family) && length(font_family) > 1) {
    font_params[["italic"]] <- font_family[["italic"]] %||% font_family[["regular"]]
    font_params[["bold"]] <- font_family[["bold"]] %||% font_family[["regular"]]
    font_params[["regular"]] <- font_family[["regular"]]
  }

  if (!all(is_ttf_path(font_params))) {
    font_params <- list(
      font = match_font(font_params[["regular"]])[["path"]],
      italic = match_font(font_params[["italic"]], italic = TRUE)[["path"]],
      bold = match_font(font_params[["bold"]], bold = TRUE)[["path"]]
    )
  }

  if (all(is_ttf_path(font_params))) {
    return(c(params, font_params))
  }

  cli_abort(
    "{.arg font_family} must be an installed TTF font or
    a vector of TTF file paths, not {font_family}.",
    call = call
  )
}

#' Test if x is a TTF file path
#' @noRd
is_ttf_path <- function(x) {
  grepl("\\.ttf$", x)
}

#' Helper function to create arguments from parameters and templates
#' @noRd
exec_d2_args <- function(params = NULL,
                         ...,
                         exec = TRUE,
                         parent_env = current_env(),
                         call = caller_env()) {
  params <- set_font_family_params(
    params = params,
    call = call
  )

  args <- glue_params(
    params = params,
    # Set list of templates to use for arguments
    templates = list(
      theme = "--theme={theme}",
      layout = "--layout={layout}",
      sketch = "--sketch={tolower(sketch)}",
      pad = "--pad={pad}",
      animate_interval = "--animate-interval={animate_interval}",
      input = "{input}",
      output = "{output}",
      font = "--font-regular={font}",
      italic = "--font-italic={italic}",
      bold = "--font-bold={bold}"
    )
  )

  if (!exec) {
    return(c(args, ...))
  }

  out <- exec_d2(args = c(args, ...))

  return(invisible(params[["output"]]))
}

#' @noRd
glue_params <- function(params, templates, parent_env = current_env()) {
  params <- list_drop_empty(params)

  if (is_empty(params)) {
    return(params)
  }

  templates <- vec_slice(
    templates,
    i = vec_in(names(templates), names(params))
  )

  vapply(
    templates,
    glue::glue,
    character(1),
    .envir = new_environment(params, parent = parent_env)
  )
}
