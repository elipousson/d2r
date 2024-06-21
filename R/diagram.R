#' Create a D2 diagram from a named vector and connectors
#'
#' Note: this is an initial experimental implementation of diagram building
#' and may change or be deprecated in the future.
#'
#' @param lines A list or character vector. Named elements are connected using
#'   the supplied `connector` value. Unnamed elements are kept as is and assumed
#'   to be valid D2 code.
#' @param ... Additional elements for diagram included after initial lines.
#' @param connector Default to `NULL` which uses `"->"` to connect the name of
#'   any shape to the value of any shape. Must be vector of:
#'   `r knitr::combine_words( keys_d2[["connector"]])`. Vector is recycled to
#'   match length of named shapes.
#' @param direction Optional direction for diagram. Must be one of:
#' `r knitr::combine_words(keys_d2[["direction"]])`.
#' @param import File name or names to append as an import to the top of the
#'   diagram. If named, the standard import method using the name of the import
#'   as the name of the imported element. If import is unnamed, the partial
#'   import method is used: https://d2lang.com/tour/imports#partial-imports See:
#'   <https://d2lang.com/tour/imports>
#' @param id Identifier for map container for diagram.
#' @inheritParams base::paste0
#' @examples
#' d2_diagram(c("x" = "y"))
#'
#' d2_diagram(c("a" = "b"), connector = "<-", direction = "up")
#'
#' d2_diagram(c("start" = "end"), connector = "<->", import = "imported.d2")
#'
#' @export
d2_diagram <- function(
    lines = NULL,
    ...,
    connector = NULL,
    id = NULL,
    direction = getOption("d2r.direction"),
    import = NULL,
    collapse = NULL) {
  if (!is.null(lines)) {
    diagram <- build_d2_diagram(lines, connector)
  }

  if (!is.null(id)) {
    diagram <- d2_container(id = id, diagram)
  }

  diagram <- c(diagram, ...)
  # diagram <- assign_shape_attributes(diagram, shapes)

  diagram <- assign_diagram_direction(diagram, direction)

  diagram <- assign_diagram_import(diagram, import)

  paste0(diagram, collapse = collapse)
}

#' Create a D2 container or map
#'
#' A container or map is a group of diagram elements. Some special chart types,
#' such as the SQL Table created by [d2_sql_table()] must be set up as a
#' container. Using containers can also help set element properties including
#' the class, label, shape and dimensions (width and height) for a set of
#' diagram elements.
#'
#' @param ... Diagram text.
#' @param lines Optional diagram text. If supplied `lines` is recycled to match
#'   the length of `id`.
#' @param id D2 diagram map identifier. All other container or map attributes
#'   are recycled to match the length of `id` using [vctrs::vec_recycle_common()]
#' @param class Diagram class
#' @param label Diagram label
#' @param shape Diagram shape
#' @param width,height Diagram width and height (in pixels?)
#' @param style A string or results from the [d2_style()] helper function.
#' @param icon Diagram icon
#' @param collapse If `collapse = "\n"`, return a string with D2 diagram code for a
#'   container. If `collapse = NULL` (default), return a character vector with the same
#'   length as id.
#' @examples
#' d2_container(
#'   "team1 <-> team2", "team2 <-> team3", "team3 <-> team1",
#'   id = c("round1", "round2")
#' )
#'
#' d2_container(
#'   lines = c("shark -> fish: eats", "lion -> gazelle: eats"),
#'   id = c("ocean", "savannah")
#' )
#'
#' @export
#' @returns A string or character vector with D2 diagram code.
d2_container <- function(
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
    collapse = NULL) {
  vec_recycle_common(
    name,
    class,
    label,
    shape,
    width,
    height,
    icon,
    style,
    .size = length(id)
  )

  # Create container vector from attributes
  container_start <- paste(
    d2_id(id, after = " {\n"),
    d2_key_val(name),
    d2_key_val(class),
    d2_key_val(label),
    d2_key_val(shape),
    d2_key_val(width),
    d2_key_val(height),
    d2_key_val(icon),
    d2_key_val(style)
  )

  container_end <- rep_along(container_start, "\n}")

  if (!is.null(lines)) {
    lines <- paste(lines, sep = "\n")

    lines <- vec_recycle(
      lines,
      size = length(container_start)
    )
  }

  paste0(
    container_start,
    lines,
    paste0(c(...), collapse = "\n"),
    container_end,
    collapse = collapse
  )
}

#' @noRd
d2_id <- function(id,
                  label = "",
                  indent = "",
                  sep = ":",
                  after = " ",
                  ...) {
  paste0(indent, id, sep, label, after, ...)
}

#' @noRd
d2_key_val <- function(
    val = NULL,
    key = arg,
    indent = "    ",
    sep = ": ",
    after = "\n",
    allow_empty = TRUE,
    allow_null = TRUE,
    allow_na = TRUE,
    arg = caller_arg(val),
    call = caller_env()) {
  if (allow_null && is.null(val)) {
    return(val)
  }

  if (allow_na && all(is.na(val))) {
    return(NULL)
  }

  if (allow_empty && identical(val, "")) {
    return(NULL)
  }

  check_character(val, allow_empty = FALSE, arg = arg, call = call)

  paste0(indent, key, sep, val, after)
}

#' Assign an import value for a diagram
#' @noRd
#' @importFrom fs is_absolute_path
assign_diagram_import <- function(
    diagram,
    import = NULL,
    call = caller_env()) {
  if (is.null(import)) {
    return(diagram)
  }

  check_d2_file(import, require_d2 = FALSE, call = call)

  import_is_absolute_path <- vapply(import, fs::is_absolute_path, logical(1))

  if (any(import_is_absolute_path)) {
    cli_abort(
      "{.arg import} can't include absolute paths.",
      call = call
    )
  }

  import_have_name <- have_name(import)

  if (any(import_have_name)) {
    import <- vec_assign(
      import,
      import_have_name,
      paste0(names(import[import_have_name]), ": @", import[import_have_name])
    )
  }

  if (any(!import_have_name)) {
    import <- vec_assign(
      import,
      !import_have_name,
      paste0("...@", import[!import_have_name])
    )
  }

  c(
    import,
    "",
    diagram
  )
}

#' Assign a direction for a diagram
#' @noRd
assign_diagram_direction <- function(
    diagram,
    direction = NULL) {
  if (is.null(direction)) {
    return(diagram)
  }
  direction <- arg_match(direction, keys_d2[["direction"]])

  c(
    paste0("direction: ", direction),
    "",
    diagram
  )
}

#' Connect shapes with connectors
#' @noRd
build_d2_diagram <- function(shapes,
                             connector = NULL,
                             call = caller_env()) {
  connector <- connector %||% "->"

  obj_check_vector(shapes, call = call)
  obj_check_vector(connector, call = call)

  if (is.list(shapes)) {
    shapes_nm <- names(shapes)
    shapes <- as.character(shapes)
    names(shapes) <- shapes_nm
  }

  shapes_have_name <- have_name(shapes)

  if (!any(shapes_have_name)) {
    return(shapes)
  }

  shapes_named <- vec_slice(shapes, shapes_have_name, error_call = call)
  shapes_names <- names(shapes_named)

  connector <- match_d2_connector(
    connector,
    size = length(shapes_named),
    error_call = call
  )

  diagram <- vec_assign(
    shapes,
    i = shapes_have_name,
    paste(shapes_names, connector, shapes_named),
    x_arg = "shapes"
  )

  assign_connector_labels(diagram, connector)
}

#' Match connector character vector to supported values
#'
#' @noRd
match_d2_connector <- function(
    connector = NULL,
    multiple = TRUE,
    size = NULL,
    default = "->",
    error_arg = caller_arg(connector),
    error_call = caller_env()) {
  connector <- connector %||% default

  connector <- arg_match(
    connector, keys_d2[["connector"]],
    multiple = multiple,
    error_arg = error_arg,
    error_call = error_call
  )

  if (is.null(size)) {
    return(connector)
  }

  vctrs::vec_recycle(
    connector,
    size = size
  )
}

#' Assign connector labels
#'
#' @noRd
assign_connector_labels <- function(diagram,
                                    connector = NULL,
                                    label = NULL) {
  label <- label %||% names(connector)

  if (is.null(label)) {
    return(diagram)
  }

  i <- !vctrs::vec_in(label, "")

  vctrs::vec_assign(
    diagram,
    i = i,
    value = paste0(
      vec_slice(diagram, i), ": ", vec_slice(label, i)
    )
  )
}

#' Assign D2 shape and object attributes
#'
#' @noRd
assign_d2_attributes <- function(diagram,
                                 obj) {
  obj_attr <- d2_shape_attributes(obj)
  missing_i <- vec_detect_missing(obj_attr)

  if (all(missing_i)) {
    return(diagram)
  }

  obj_attr <- vec_slice(obj_attr, !missing_i)
  obj <- vec_slice(obj, !missing_i)

  # TODO: Append attribute assignments
  # c(
  #   diagram,
  #   obj
  # )
}

#' @noRd
d2_shape_attributes <- function(shapes) {
  lapply(shapes, attributes)
}

#' @noRd
get_d2_attr <- function(x, which = "shape") {
  attr(x, which)
}

#' @noRd
set_d2_attr <- function(x, value, which = "shape") {
  attr(x, which) <- value
  x
}
