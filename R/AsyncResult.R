#' @importFrom R.oo setConstructorS3
#' @importFrom R.methodsS3 setMethodS3
#' @importFrom R.oo extend
setConstructorS3("AsyncResult", function(reg, id=1L, ...) {
  extend(list(reg=reg, id=id, ...), "AsyncResult")
})


#' @export
#' @export await
#' @importFrom R.methodsS3 throw
#' @importFrom R.utils mprint mprintf mstr
#' @importFrom BatchJobs getStatus getErrorMessages loadResult
await.AsyncResult <- function(...) NULL; rm("await.AsyncResult")
setMethodS3("await", "AsyncResult", function(object, cleanup=FALSE, maxTries=10L, interval=getOption("async::pollinterval", 1.0), timeout=+Inf, ...) {
  throw <- R.methodsS3::throw

  debug <- getOption("async::debug", FALSE)
  if (debug) mprintf("Polling...")

  reg <- object$reg
  id <- object$id

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
    throw(sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchroneous evaluation is still running: %s", times, interval, reg))
  }

  ## Cleanup
  if (cleanup) {
    ## FIXME: Completely remove the registry
  }

  res
}) # await()
