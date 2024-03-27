#' Create a D2 style
#'
#' Learn more: <https://d2lang.com/tour/style>
#'
#' @param ... Named attributes for style. Underscores in parameter names are
#'   substituted for dashes to match D2 style, e.g. `"stroke_dash"` is converted
#'   to `"stroke-dash"`.
#' @param level String. "shape", "connector", or "root". If "root", limit style to
#'   supported attributes for root-level style. See
#'   <https://d2lang.com/tour/style#root> for more information.
#' @param indent String. Only used if `inline = TRUE`.
#' @param inline If `TRUE` or a string value , return a string instead of an array. Errors in either case
#'   and more than a single parameter is supplied. If string, the string is
#'   assumed to be the id for a shape, connector, or container/map and appended
#'   before the "style" keyword.
#' @examples
#' d2_style(stroke = "#f4a261", inline = TRUE)
#'
#' d2_style(fill = "green", inline = "tree")
#'
#' d2_style(animated = TRUE, stroke_dash = 3, level = "connector")
#'
#' @export
d2_style <- function(
    ...,
    inline = FALSE,
    level = c("shape", "connector", "root"),
    after = "\n") {
  params <- list2(...)

  if (is_empty(params)) {
    return(NULL)
  }

  params <- match_d2_style(
    params,
    level = level
  )

  if (is_string(inline) || is_true(inline)) {
    vec_check_size(params, size = 1)

    indent <- "style."

    if (is_string(inline)) {
      indent <- paste0(inline, ".", indent)
    }

    style <- d2_key_val(
      params[[1]],
      key = names(params),
      indent = indent,
      after = after
    )

    return(style)
  }

  style <- d2_key_val(
    val = params,
    key = names(params),
    after = after
  )

  paste0(
    c("style: {\n", style, "}"),
    collapse = ""
  )
}

#' @noRd
match_d2_style <- function(
    params,
    level = c("shape", "connector", "root"),
    error_call = caller_env()) {
  obj_check_vector_named(params, error_call = error_call)

  nm <- sub("_", "-", names(params))
  params <- set_names(params, nm)

  values <- keys_d2[["style"]]
  level <- arg_match(level, error_call = error_call)

  if (level == "shape") {
    values <- setdiff(values, "animated")
  } else if (level == "connector") {
    values <- setdiff(
      values,
      c("fill", "fill-pattern", "shadow", "multiple", "3D", "double-border")
    )
  } else if (level == "root") {
    values <- c(
      "fill", "fill-pattern", "stroke",
      "stroke-width", "stroke-dash", "double-border"
    )
  }

  arg_match(nm, values = values, multiple = TRUE, error_call = error_call)

  params
}
