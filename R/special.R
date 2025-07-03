#' @noRd
d2_tempid <- function(..., size = 8, replace = TRUE) {
  paste0(
    c(..., sample(c(LETTERS, letters), size, replace = replace)),
    collapse = ""
  )
}

#' Create a D2 SQL Table Diagram
#'
#' @param data A data frame used for column names and types. Ignored if
#'   `col_types` is a named vector. Required if `col_types` is unnamed or a
#'   function.
#' @param col_types A named character vector, an unnamed character vector with
#'   the same length as the number of columns in `data`, or a function that
#'   returns a character vector based on the columns from `data`. Defaults to
#'   [vctrs::vec_ptype_abbr]. Other recommended options are
#'   [vctrs::vec_ptype_full] or [base::typeof]. If `types` is a named character
#'   vector, the column names of `data` are ignored the names of types used
#'   instead.
#' @param visibility One or more of: `r d2r::keys_d2[["visibility"]]`. Length is
#'   recycled to match the number of columns in `data`.
#' @param id Diagram container id passed to [d2_container()]. Defaults to
#'   `NULL`. If `NULL`, `id` is set to `rlang::caller_arg(data)` if `data` is
#'   supplied or a random string if `data = NULL`.
#' @param label Diagram label. Defaults to `id`. If supplied, `label` is
#'   displayed instead of `id`.
#' @inheritDotParams d2_container -shape
#' @examples
#'
#' d2_sql_table(mtcars[, 1:4])
#'
#' d2_sql_table(
#'   label = "col_types Example",
#'   col_types = c("name" = "char", "num" = "int")
#' )
#'
#' @returns A character string with D2 diagram code using the "sql_type"
#'   shape.
#' @export
d2_sql_table <- function(
  data = NULL,
  visibility = NULL,
  col_types = vec_ptype_abbr,
  id = NULL,
  label = id,
  ...
) {
  if (is_function(col_types)) {
    col_types <- vapply(data, col_types, character(1))
  }

  if (!is_named(col_types)) {
    col_types <- set_names(col_types, names(data))
  }

  if (!is.null(data) && is.null(id)) {
    data_arg <- caller_arg(data)
    # Sanitize data argument
    if (any(grepl('"', data_arg))) {
      id <- paste0("'", data_arg, "'")
    } else {
      id <- paste0('"', data_arg, '"')
    }
  } else {
    id <- id %||% d2_tempid()
  }

  check_string(id, allow_empty = FALSE)

  visibility <- vec_recycle(
    visibility %||% "",
    size = length(col_types)
  )

  visibility <- arg_match(
    visibility,
    values = d2r::keys_d2[["visibility"]],
    multiple = TRUE
  )

  visibility <- sub("#", "\\#", visibility)

  d2_container(
    d2_key_val(val = col_types, key = paste0(visibility, names(col_types))),
    id = id,
    label = label,
    shape = "sql_table",
    ...
  )
}
