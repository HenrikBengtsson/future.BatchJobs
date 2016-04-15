#' Exception class for BatchJobsFuture-related errors
#'
#' @param \ldots Passed to \code{\link[R.oo]{Exception}}
#' @param task The BatchJobs future for which the error occurred.
#'
#' @export
#' @importFrom R.oo setConstructorS3
#' @importFrom R.oo extend
#' @importFrom R.oo Exception
#' @keywords internal
BatchJobsFutureError <- function(..., task=NULL) NULL  ## To please roxygen2
setConstructorS3("BatchJobsFutureError", function(..., task=NULL) {
  error <- extend(Exception(...), "BatchJobsFutureError")
  error$task <- task
  error
})

#' @export
#' @importFrom R.oo getMessage
getMessage.BatchJobsFutureError <- function(x, ...) {
  msg <- R.oo::getMessage.Exception(x, ...)
  task <- x$task
  if (!is.null(task)) {
    info <- captureOutput(print(task))
    info <- trim(info)
    info <- paste(info, collapse="; ")
    msg <- sprintf("%s [DEBUG INFORMATION: %s]", msg, info)
  }
  msg
}
