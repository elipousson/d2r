#' Install D2 using the sh script
#'
#' [d2_install()] downloads and executes a `sh` script provided for installing
#' D2 in [the Getting Started documentation](https://d2lang.com/tour/install).
#' This is not the most secure way to install D2. You should consider using
#' Homebrew, a pre-built Windows installer, or another installation option
#' listed in [the detailed installation
#' instructions](https://github.com/terrastruct/d2/blob/master/docs/INSTALL.md).
#'
#' @param dry_run If `TRUE` (default), append `"--dry-run"` to the arguments
#'   passed to the `sh` command.
#' @export
d2_install <- function(dry_run = TRUE) {
  exec_wait("curl", args = "--fsSL https://d2lang.com/install.sh")

  args <- "-s --"

  if (dry_run) {
    args <- c(args, "--dry-run")
  }

  exec_wait("sh", args = args)
}
