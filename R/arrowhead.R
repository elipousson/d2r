#' Specify attributes for a single arrowhead or a pair of source and target
#' arrowheads
#'
#' [d2_arrowhead()] specifies the label, shape, and fill for a source or target
#' arrowhead. [d2_arrowheads()] allows the specification of both a source and
#' target arrowhead.
#'
#' @param label Short arrowhead label.
#' @param shape See `keys_d2[["arrowhead"]]` for allowed shape values.
#' @param filled Must be `NULL` or logical. Ignored if shape is `"arrow"` or a
#'   crow's foot shape (any shape starting with `"cf-"`).
#' @param type Type of arrowhead: "target" or "source".
#' @examples
#' d2_arrowhead(
#'   shape = "cf-many-required",
#'   label = "Many required"
#'  )
#'
#' example_diagram <- "A -> B"
#'
#' d2_arrowheads(
#'   shape = "triangle",
#'   id = example_diagram,
#'   filled = FALSE
#' )
#'
#' d2_arrowheads(
#'   source = list(
#'     shape = "triangle",
#'     label = "source",
#'     filled = FALSE
#'    ),
#'   target = list(
#'     shape = "circle",
#'     filled = TRUE
#'   ),
#'   id = example_diagram
#'  )
#'
#' @export
d2_arrowhead <- function(shape = "triangle",
                         type = c("target", "source"),
                         label = NULL,
                         filled = NULL) {
  type <- arg_match(type)

  type <- paste0(type, "-arrowhead")

  shape <- arg_match(
    shape,
    values = keys_d2[["arrowhead"]]
  )

  if (!is.null(filled)) {
    if (shape %in% keys_d2[["arrowhead"]][c(1,3,4)]) {
      check_logical(filled)
      filled <- tolower(filled)
    } else {
      filled <- NULL
    }
  }

  if (shape == "triangle") {
    shape <- NULL
  }

  arrowhead_spec <- list_drop_empty(
    list(
      "shape: " = shape,
      "style.filled: " = filled
    )
  )

  if (is_empty(arrowhead_spec)) {
    if (is.null(label)) {
      return(NULL)
    }

    return(paste0(type, ".label: ", label))
  }

  if (!is.null(label)) {
    label <- paste0(label, " ")
  } else if (is.null(filled)) {
    return(paste0(type, ".shape: ", shape))
  }

  paste0(
    c(
      paste0(type, ": ", label, "{"),
      paste0(names(arrowhead_spec), arrowhead_spec),
      "}"
    ),
    collapse = "\n"
  )
}

#' @name d2_arrowheads
#' @rdname d2_arrowhead
#' @param ... Named parameters used to set values for both the source and
#'   target arrowheads.
#' @param target,source Named list of arguments (excluding type) passed to
#'   [d2_arrowhead()].
#' @inheritParams d2_container
#' @importFrom utils modifyList
#' @export
d2_arrowheads <- function(...,
                          target = list(),
                          source = list(),
                          id = NULL) {
  params <- list2(...)

  if (!is_empty(params)) {
    params[["type"]] <- NULL
    target <- utils::modifyList(params, target)
    source <- utils::modifyList(params, source)
  }

  if (!is_empty(target)) {
    target <- utils::modifyList(target, list(type = "target"))
    target <- exec(d2_arrowhead, !!!target)
  }

  if (!is_empty(source)) {
    source <- utils::modifyList(source, list(type = "source"))
    source <- exec(d2_arrowhead, !!!source)
  }

  if (is_empty(c(target, source))) {
    return(NULL)
  }

  arrowhead_spec <- paste(
    source,
    target,
    sep = "\n"
  )

  if (is.null(id)) {
    return(arrowhead_spec)
  }

  d2_container(
    arrowhead_spec,
    id = id
  )
}
