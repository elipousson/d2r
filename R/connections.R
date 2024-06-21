#'
#' @noRd
as_d2_vector <- function(
    x,
    allow_null = TRUE,
    na = "drop",
    arg = caller_arg(x),
    call = caller_env()) {
  if (is.list(x)) {
    list_check_all_size(x, 1, arg = arg, call = call)
    x <- vapply(x, as.character, character(1))
  }

  # Handle NA values
  na <- arg_match0(na, values = c("drop", "error"), error_call = call)
  has_na_values <- any(is.na(x))
  has_missing_nm <- any(have_name(x)) && !is_named(x)

  if (na == "drop" && has_na_values) {
    cli::cli_alert_warning(
      "Dropping `NA` values from {.arg {arg}}."
    )

    x <- vec_slice(x, i = !is.na(x))

    if (has_missing_nm) {
      cli::cli_alert_warning(
        "Dropping values with missing names from {.arg {arg}}."
      )

      x <- vec_slice(x, i = have_name(x))
    }
  } else if (na == "error") {
    info_message <- c(
      "i" = 'Set {.code na = "drop"} to allow missing values to be dropped.'
    )

    if (has_na_values) {
      cli_abort(
        c(
          "{.arg {arg}} can't contain {.code NA} values.",
          info_message
        ),
        call = call
      )
    }

    if (has_missing_nm) {
      cli_abort(
        c(
          "{.arg {arg}} is named but some names area missing.",
          info_message
        ),
        call = call
      )

      x <- vec_slice(x, i = have_name(x))
    }
  }

  x
}

#' Build connections between two vectors or using one named vector or list
#' @noRd
d2_connections <- function(from = NULL,
                           to = NULL,
                           connector = NULL,
                           label = NULL,
                           na = "drop",
                           recycle = FALSE,
                           .size = NULL,
                           call = caller_env()) {
  from <- as_d2_vector(from, na = na)

  if (is_named(from)) {
    to <- from
    from <- names(from)
  } else {
    to <- as_d2_vector(to, na = na)
  }

  if (recycle) {
    args <- vec_recycle_common(
      to = to,
      from = from,
      .size = .size
    )

    to <- args[["to"]]
    from <- from[["from"]]
  } else if (!is.null(.size)) {
    vec_check_size(from, size = size, call = call)
    vec_check_size(to, size = size, call = call)
  }

  connector <- match_d2_connector(
    connector,
    size = length(from),
    error_call = call
  )

  diagram <- paste(from, connector, to)

  assign_connector_labels(diagram, connector, label = label)
}
