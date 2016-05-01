#' Exception class for BatchJobsFuture-related errors
#'
#' @param \ldots Passed to \code{\link[R.oo]{Exception}}
#' @param future The BatchJobs future for which the error occurred.
#'
#' @export
#' @importFrom R.oo setConstructorS3
#' @importFrom R.oo extend
#' @importFrom R.oo Exception
#'
#' @aliases getOutput
#'
#' @keywords internal
BatchJobsFutureError <- function(..., future=NULL) NULL  ## To please roxygen2
setConstructorS3("BatchJobsFutureError", function(..., future=NULL, output=NULL) {
  error <- extend(Exception(...), "BatchJobsFutureError")
  error$future <- future
  error$output <- output
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


#' @export
getOutput.BatchJobsFutureError <- function(x, collapse=NULL, head=NULL, tail=NULL, ...) {
  output <- x$output

  ## Return "as is"?
  if (is.null(collapse) && is.null(head) && is.null(tail)) return(output)

  ## Truncate?
  if (!is.null(head) && !is.null(tail)) {
    idxs <- seq_along(output)
    idxs <- sort(unique(c(head(idxs, n=head), tail(idxs, n=tail))))
    output <- output[idxs]
    idxs
  } else if (!is.null(head)) {
    output <- head(output, n=head)
  } else if (!is.null(tail)) {
    output <- tail(output, n=tail)
  }

  ## Collapse? (add line endings)
  if (!is.null(collapse)) output <- paste(output, collapse=collapse)

  output
}

#' @export
#' @keywords internal
getOutput <- function(...) UseMethod("getOutput")
