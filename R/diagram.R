#' Create a D2 diagram from a named vector and connectors
#'
#' [d2_diagram()] is an initial experimental implementation of diagram building
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
#' @param .id Identifier for map container for diagram.
#' @inheritParams base::paste0
#' @examples
#' d2_diagram(c("x" = "y"))
#'
#' d2_diagram(c("a" = "b"), "<-", direction = "up")
#'
#' d2_diagram(c("start" = "end"), "<->", import = "imported.d2")
#'
#' @export
d2_diagram <- function(
    lines = NULL,
    ...,
    connector = NULL,
    .id = NULL,
    direction = getOption("d2r.direction"),
    import = NULL,
    collapse = NULL) {
  if (!is.null(lines)) {
    diagram <- build_d2_diagram(lines, connector)
  }

  if (!is.null(.id)) {
    diagram <- d2_map(.id = .id, diagram)
  }

  diagram <- c(diagram, ...)
  # diagram <- assign_shape_attributes(diagram, shapes)

  diagram <- assign_diagram_direction(diagram, direction)

  diagram <- assign_diagram_import(diagram, import)

  paste0(diagram, collapse = collapse)
}

#' Create a D2 map
#'
#' A map is a group within the diagram. This is required for [d2_sql_table()]
#' but can to set properties such as the class, label, or shape for a grouped
#' set of diagram elements.
#'
#' @param ... Text lines for diagram.
#' @param .id D2 diagram map identifier.
#' @param .class Diagram map class
#' @param .label Diagram map label
#' @param .shape Diagram map shape
#' @examples
#' d2_map(
#'   "principal -> teachers",
#'   "teachers -> students",
#'   .id = "school",
#'   .shape = "circle"
#' )
#'
#' @export
d2_map <- function(...,
                   .id = NULL,
                   .class = NULL,
                   .label = NULL,
                   .shape = NULL) {
  paste0(
    c(
      paste0(
        d2_key_val(val = "", key = .id, indent = ""), "{"
      ),
      d2_key_val(.class, "class"),
      d2_key_val(.label, "label"),
      d2_key_val(.shape, "shape"),
      ...,
      "}"
    ),
    collapse = "\n"
  )
}

#' @noRd
d2_key_val <- function(val = NULL, key = NULL, indent = "    ", sep = ":") {
  if (is.null(val)) {
    return(val)
  }

  paste0(indent, key, sep, " ", val)
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

  connector <- match_d2_connector(
    connector,
    size = length(shapes_named),
    error_call = call
  )

  diagram <- vec_assign(
    shapes,
    i = shapes_have_name,
    paste(names(shapes_named), connector, shapes_named),
    x_arg = "shapes"
  )

  assign_connector_labels(diagram, connector)
}

#' Match connector character vector to supported values
#'
#' @noRd
match_d2_connector <- function(
    connector,
    multiple = TRUE,
    size = NULL,
    error_arg = caller_arg(connector),
    error_call = caller_env()) {
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
                                    connector = NULL) {
  connector_nm <- names(connector)

  if (is.null(connector_nm)) {
    return(diagram)
  }

  i <- !vctrs::vec_in(connector_nm, "")

  vctrs::vec_assign(
    diagram,
    i = i,
    value = paste0(
      vec_slice(diagram, i), ": ", vec_slice(connector_nm, i)
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

#' Check if object is a vector with only named elements
#'
#' @noRd
obj_check_vector_named <- function(
    x,
    ...,
    arg = caller_arg(x),
    call = caller_env()) {
  vctrs::obj_check_vector(x, arg = arg, call = call)

  if (!is_named(x)) {
    cli_abort(
      "{.arg {arg}} must be a named vector",
      call = call
    )
  }
}
