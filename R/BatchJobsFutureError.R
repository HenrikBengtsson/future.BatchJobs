#' Exception class for BatchJobsFuture-related errors
#'
#' @param \ldots Passed to \code{\link[R.oo]{Exception}}
#' @param future The BatchJobs future for which the error occurred.
#'
#' @export
#' @importFrom R.oo setConstructorS3
#' @importFrom R.oo extend
#' @importFrom R.oo Exception
#' @keywords internal
BatchJobsFutureError <- function(..., future=NULL) NULL  ## To please roxygen2
setConstructorS3("BatchJobsFutureError", function(..., future=NULL) {
  error <- extend(Exception(...), "BatchJobsFutureError")
  error$future <- future
  error
})

#' @export
#' @importFrom R.oo getMessage
getMessage.BatchJobsFutureError <- function(x, ...) {
  msg <- R.oo::getMessage.Exception(x, ...)
  future <- x$future
  if (!is.null(future)) {
    info <- captureOutput(print(future))
    info <- trim(info)
    info <- paste(info, collapse="; ")
    msg <- sprintf("%s [DEBUG INFORMATION: %s]", msg, info)
  }
  msg
}
