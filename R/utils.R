.onLoad <- function(lib, pkg) {
  utils::data(
    list = c("themes_d2", "keys_d2"),
    package = pkg,
    envir = parent.env(environment())
  )
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
