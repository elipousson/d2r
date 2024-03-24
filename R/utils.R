#' Read, write, and format D2 files
#'
#' @param file A file path with a d2 file extension.
#' @export
d2_read <- function(file) {
  # check file path
  check_d2_file(file)

  # read a d2 file from disk
  readLines(file, warn = FALSE)
}

#' @param data A set of lines or a file path to a d2 file.
#' @rdname d2_read
#' @param file File name to write to disk.
#' @param fileext File extension to use for writing data to disk.
#' @export
d2_write <- function(data,
                     file = tempfile(fileext = fileext),
                     fileext = "d2") {
  if (all(is_d2_file(data, fileext))) {
    data <- d2_read(data)
  }

  # write a d2 file to disk
  writeLines(data, file)

  invisible(file)
}

#' @rdname d2_read
#' @export
d2_fmt <- function(file) {
  check_d2_file(file)
  out <- exec_d2("fmt", args = file, std_out = std_bullets)
}


#' Basic D2 utilities
#'
#'
#' @name d2-utils
NULL

#' - [d2_version()]: List the version of D2 (if installed)
#'
#' @rdname d2-utils
#' @export
d2_version <- function() {
  std_out <- \(x) {
    std_alert_info(
      x,
      before = "v",
      after = paste0(" {.url https://d2lang.com/releases/", as_text(x), "}")
    )
  }

  out <- exec_d2("version", std_out = std_out)
}

#' - [d2_themes()]: List available D2 themes
#'
#' @rdname d2-utils
#' @export
d2_themes <- function() {
  out <- exec_d2("themes", std_out = std_bullets)
}

#' - [d2_layout()]: Lists available layout engine options with short help
#'
#' @rdname d2-utils
#' @param layout Layout name. If supplied, display long help for a particular layout engine,
#' including its configuration options.
#' @export
d2_layout <- function(layout = NULL) {
  out <- exec_d2(c("layout", layout), std_out = std_bullets)
}

#' - [is_d2_file()]: Test for a `".d2"` file extension
#'
#' @rdname d2-utils
#' @export
is_d2_file <- function(file, fileext = "d2") {
  if (length(fileext) > 1) {
    fileext <- paste0("\\.", fileext, collapse = "|")
    fileext <- paste0("(", fileext, ")")
  } else {
    fileext <- paste0("\\.", fileext)
  }

  grepl(fileext, file)
}

#' @noRd
check_d2_file <- function(
    file,
    allow_null = FALSE,
    allow_empty = FALSE,
    require_d2 = TRUE,
    arg = caller_arg(file),
    call = caller_env()) {
  check_character(file, allow_null = allow_null, allow_empty = allow_empty)

  allow_null <- allow_null && is.null(file)
  allow_empty <- allow_empty && identical(file, "")
  allow_files <- all(is_d2_file(file)) || !require_d2

  if (allow_null || allow_empty || allow_files) {
    return(invisible(NULL))
  }

  cli_abort(
    "{.arg {arg}} must be a character vector ending with the file extension {.str .d2}.",
    call = call
  )
}

#' @noRd
exec_d2 <- function(args = list(), sep = "", std_out = std_alert_info) {
  exec_wait("d2", args = args, std_out = std_out)
}

#' @noRd
std_alert_info <- function(x, before = "", after = "", bullets  = "i") {
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
