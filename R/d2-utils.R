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
#' @param file File name to write to disk. If `NULL`, set to temporary file name
#'   using supplied file extension.
#' @param fileext File extension to use for writing data to disk.
#' @param tidy If `TRUE`, pass the tidy flag to the command line execution.
#'   Defaults to `getOption("d2r.tidy", TRUE)`.
#' @rdname d2_read
#' @export
d2_write <- function(
  data,
  file = NULL,
  fileext = "d2",
  tidy = getOption("d2r.tidy", TRUE)
) {
  if (all(is_d2_file(data, fileext))) {
    data <- d2_read(data)
  }

  file <- file %||% tempfile(fileext = paste0(".", fileext))

  # write a d2 file to disk
  writeLines(data, file)

  if (tidy) {
    d2_fmt(file = file)
  }

  invisible(file)
}

#' @rdname d2_read
#' @export
d2_fmt <- function(file) {
  check_d2_file(file)
  out <- exec_d2(args = c("fmt", file), std_out = std_bullets)
}

#' Basic D2 utilities
#'
#' @name d2-utils
NULL

#' - [d2_which()]: List path for D2
#'
#' @rdname d2-utils
#' @export
d2_which <- function() {
  system2("which", "d2", stdout = TRUE)
}

#' - [d2_version()]: List the installed version of D2
#'
#' @rdname d2-utils
#' @export
d2_version <- function() {
  v <- system2("d2", "version", stdout = TRUE)
  cli_alert_info(
    paste0("v", v, " {.url https://d2lang.com/releases/", v, "}")
  )

  invisible(v)
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

#' - [d2_validate()]: List the installed version of D2
#'
#' @rdname d2-utils
#' @export
d2_validate <- function() {
  v <- system2("d2", "version", stdout = TRUE)
  cli_alert_info(
    paste0("v", v, " {.url https://d2lang.com/releases/", v, "}")
  )

  invisible(v)
}

#' Does file use a d2 extension?
#'
#' [is_d2_file()] tests for a `".d2"` file extension. A d2 file extension is not
#' required in all cases but most functions in this package expect a d2
#' extension by default.
#'
#' @param file File name(s) to test.
#' @param fileext File extension(s) to test for.
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
  call = caller_env()
) {
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

#' Allowed parameters
#'
#' @param h,host listening address when used with watch (default "localhost")
#' @param p,port port listening address when used with watch (default "0")
#' @noRd
exec_d2 <- function(args = list(), std_out = std_alert_info) {
  std_out <- std_out %||% TRUE
  exec_wait(cmd = "d2", args = args, std_out = std_out)
}
