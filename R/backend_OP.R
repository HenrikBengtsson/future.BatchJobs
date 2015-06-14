#' Evaluate asynchroneous expression on a specific backend
#'
#' @usage x %<=% { expr } %backend% backend
#'
#' @export
#' @importFrom R.utils mprintf
`%backend%` <- function(x, y) {
  lhs <- substitute(x)
  backend <- y
  envir <- parent.frame(1)

  ## Temporary use a different backend
  obackend <- backend(NULL)
  on.exit(backend(obackend, quietly=TRUE))
  what <- backend(backend)

  debug <- getOption("async::debug", FALSE)
  if (debug) {
    mprintf("Using backend: '%s'\n", what)
    mprintf("Previous backend: '%s'\n", obackend)
  }

  eval(lhs, envir=envir)
}
