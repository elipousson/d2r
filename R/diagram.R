#' Format a vectors of shapes and connectors as a D2 diagram
#'
#' @export
d2_diagram <- function(
    shapes,
    connector = NULL,
    class = NULL,
    direction = NULL,
    import = NULL,
    ...) {

  diagram <- build_d2_diagram(shapes, connector)

  # diagram <- assign_shape_attributes(diagram, shapes)

  diagram <- assign_diagram_direction(diagram, direction)

  diagram <- assign_diagram_import(diagram, import)

  diagram
}

#' Assign an import value for a diagram
#' @noRd
assign_diagram_import <- function(
    diagram,
    import = NULL,
    call = caller_env()) {
  if (is.null(import)) {
    return(diagram)
  }

  check_d2_file(import, call = call, require_d2 = FALSE)

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
    direction = NULL
) {
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
    error_call = call)

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
