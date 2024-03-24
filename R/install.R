#' Install D2 using the sh script
#'
#' This does not work
#' @export
d2_install <- function(dry_run = TRUE) {
  exec_wait("curl", args = "--fsSL https://d2lang.com/install.sh")

  args <- "-s --"

  if (dry_run) {
    args <- paste(args, "--dry-run")
  }

  exec_wait("sh", args = args)
}
