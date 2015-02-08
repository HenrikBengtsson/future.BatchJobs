AsyncResult <- function(reg, id=1L, ...) {
  structure(list(reg=reg, id=id, ...), class=c("AsyncResult", "list"))
}


#' Retrieves the value of of the asynchronously evaluated expression
#'
#' @param ... Arguments passed to S3 method.
#'
#' @return The value of the evaluated expression.
#' If an error occurs, an informative Exception is thrown.
#'
#' @export
await <- function(...) UseMethod("await")


#' Retrieves the value of of the asynchronously evaluated expression
#'
#' @param task The asynchronously task
#' @param cleanup If TRUE, the registry is completely removed upon
#' success, otherwise not.
#' @param maxTries The number of polls before giving up.
#' @param interval The number of seconds to wait between polls.
#' @param ... Not used.
#'
#' @return The value of the evaluated expression.
#' If an error occurs, an informative Exception is thrown.
#'
#' @export
#' @importFrom R.methodsS3 throw
#' @importFrom R.utils mprint mprintf mstr
#' @importFrom BatchJobs getStatus getErrorMessages loadResult
await.AsyncResult <- function(task, cleanup=FALSE, maxTries=10L, interval=getOption("async::pollinterval", 1.0), ...) {
  throw <- R.methodsS3::throw

  debug <- getOption("async::debug", FALSE)
  if (debug) mprintf("Polling...")

  reg <- task$reg
  id <- task$id

  finished <- FALSE
  status <- NULL
  tries <- 1L
  while (tries <= maxTries) {
    status <- getStatus(reg, ids=id)
    status <- (unlist(status) == 1L)
    statusT <- status[c("done", "error", "expired")]
    if (any(statusT)) {
      finished <- TRUE
      break
    }
    if (debug) { mprintf("\n"); mprint(status) }
    Sys.sleep(interval)
    tries <- tries + 1L
  }

  res <- NULL
  if (finished) {
    if (debug) { mprint("Results:"); mstr(res) }
    if (status["error"]) {
      cleanup <- FALSE
      msg <- getErrorMessages(reg, ids=id)
      msg <- paste(sQuote(msg), collapse=", ")
      throw("BatchJobError: ", msg)
    } else if (status["expired"]) {
      cleanup <- FALSE
      msg <- "<expired>"
      throw("BatchJobExpiration: ", msg)
    } else if (status["done"]) {
      res <- loadResult(reg, id=id)
    }
  } else {
    throw(sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchroneous evaluation is still running: %s", tries, interval, reg))
  }

  ## Cleanup
  if (cleanup) {
    ## FIXME: Completely remove the registry
  }

  res
} # await()
