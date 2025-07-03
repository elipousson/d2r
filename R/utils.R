#' Convert a list to character elements
#' @noRd
list_as_chr <- function(x, use_names = TRUE) {
  vapply(x, as.character, character(1), USE.NAMES = use_names)
}

#' Check if object is a vector with only named elements
#' @noRd
obj_check_vector_named <- function(
  x,
  ...,
  nm = NULL,
  allowed_any = FALSE,
  allowed_only = !allowed_any,
  arg = caller_arg(x),
  call = caller_env()
) {
  vctrs::obj_check_vector(x, arg = arg, call = call)

  supplied_nm <- !is.null(nm)
  x_has_nm <- has_name(x, nm)

  allowed <- c(
    all(x_has_nm),
    # FIXME: add error message for allowed_only
    allowed_only && all(names(x) %in% nm),
    allowed_any && any(x_has_nm)
  )

  if (any(allowed)) {
    return(invisible(NULL))
  }

  if (!supplied_nm && is_named(x)) {
    return(invisible(NULL))
  }

  msg <- "{.arg {arg}} must be a named vector"
  what <- "all of"

  if (allowed_any) {
    what <- "any of"
  }

  if (supplied_nm) {
    msg <- "{.arg {arg}} must be a named vector with {what} the names {.str {nm}}"
  }

  cli_abort(
    msg,
    call = call
  )
}

#' @noRd
std_alert_info <- function(x, before = "", after = "", bullets = "i") {
  message <- set_names(
    paste0(before, as_text(x), after),
    bullets
  )

  cli_alert_info(message)
}

#' @noRd
std_bullets <- function(x) {
  cli_bullets(as_text(x))
}
