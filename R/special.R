tempid <- function(..., size = 8, replace = TRUE) {
  paste0(c(..., sample(c(LETTERS, letters), size, replace = replace)), collapse = "")
}

#' Create a D2 SQL Table Diagram
#'
#' @param data A data frame used for column names and types.
#' @param types A character vector matching the number of columns in data or a
#'   function used to return the character vector. Defaults to
#'   [vctrs::vec_ptype_abbr]. Other recommended options are
#'   [vctrs::vec_ptype_full] or [base::typeof]. If `types` is a named character
#'   vector, the column names of `data` are ignored the names of types used
#'   instead.
#' @param visibility One or more of: `r keys_d2[["visibility"]]`. Length is
#'   recycled to match the number of columns in `data`.
#' @param .id Table ID passed to [d2_map()]. Defaults to
#'   `rlang::caller_arg(data)`
#' @param ... Unused at present.
#' @export
d2_sql_table <- function(
    data = NULL,
    visibility = NULL,
    types = vec_ptype_abbr,
    .id = caller_arg(data),
    ...) {
  if (is_function(types)) {
    types <- lapply(data, types)
  }

  if (!is_named(types)) {
    types <- set_names(types, names(data))
  }

  visibility <- vec_recycle(
    visibility %||% "",
    size = length(types)
  )

  visibility <- arg_match(visibility, keys_d2[["visibility"]], multiple = TRUE)
  visibility <- sub("#", "\\#", visibility)

  d2_map(
    paste0(
      "    ", visibility, names(types), ": ", types
    ),
    .id = .id,
    .shape = "sql_table"
  )
}
