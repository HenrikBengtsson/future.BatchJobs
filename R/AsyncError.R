#' Exception class for async-related errors
#'
#' @usage AsyncError(...)
#' @param \ldots Passed to \code{\link[R.oo]{Exception}}
#'
#' @aliases AsyncTaskError
#' @export
#' @importFrom R.oo setConstructorS3
#' @importFrom R.oo extend
#' @importFrom R.oo Exception
#' @keywords internal
AsyncError <- function(...) NULL  ## To please roxygen2
setConstructorS3("AsyncError", function(...) {
  extend(Exception(...), "AsyncError")
})

#' @export
#' @importFrom R.oo setConstructorS3
#' @importFrom R.oo extend
#' @importFrom R.oo Exception
#' @keywords internal
AsyncTaskError <- function(...) NULL  ## To please roxygen2
setConstructorS3("AsyncTaskError", function(..., task=NULL) {
  error <- extend(AsyncError(...), "AsyncTaskError")
  error$task <- task
  error
})

#' @export
#' @importFrom R.oo getMessage
getMessage.AsyncTaskError <- function(x, ...) {
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
