#' A asynchroneous task
#'
#' @param expr A R expression.
#' @param reg A BatchJob Registry
#' @param id A BatchJob Registry ID.
#' @param ... Not used.
#'
#' @return An AsyncTask object
#'
#' @export
AsyncTask <- function(expr=NULL, reg=NA, id=1L, ...) {
  exprT <- substitute(expr)
  obj <- list(
    expr=exprT,
    backend=list(reg=reg, id=id)
  )
  structure(obj, class=c("AsyncTask"))
}

#' Print an AsyncTask
#'
#' @param x An AsyncTask object
#' @param ... Not used.
#'
#' @export
#' @importFrom R.utils captureOutput
#' @importFrom R.utils printf
print.AsyncTask <- function(x, ...) {
  printf("%s:\n", class(x)[1])
  printf("Expression:\n")
  code <- captureOutput(print(x$expr))
  code <- paste(sprintf("  %s", code), collapse="\n")
  printf("%s\n", code)
  stat <- status(x)
  printf("Status: %s\n", paste(sQuote(stat), collapse=", "))
  if ("error" %in% stat) printf("Error: %s\n", error(x))
  printf("Backend:\n")
  backend <- x$backend
  print(backend$reg)
}


#' Status of an AsyncTask
#'
#' @param ... Arguments passed to the S3 method
#'
#' @return A character vector.
#'
#' @export
#' @export finished
#' @export value
#' @export error
status <- function(...) UseMethod("status")
finished <- function(...) UseMethod("finished")
value <- function(...) UseMethod("value")
error <- function(...) UseMethod("error")

#' Status of an AsyncTask
#'
#' @param obj The asynchronously task
#' @param ... Not used.
#'
#' @return A character vector.
#'
#' @export
status.AsyncTask <- function(obj, ...) {
  backend <- obj$backend
  reg <- backend$reg
  if (!inherits(reg, "Registry")) return(NA_character_)

  id <- backend$id
  status <- getStatus(reg, ids=id)
  status <- (unlist(status) == 1L)
  status <- status[status]
  status <- sort(names(status))
  status
} # status()


#' @export
finished.AsyncTask <- function(obj, ...) {
  any(c("done", "error", "expired") %in% status(obj))
}


#' @export
value.AsyncTask <- function(obj, ...) {
  stat <- status(obj)
  if (!"done" %in% stat) {
    throw(sprintf("%s did not succeed: %s", class(obj)[1L], paste(sQuote(stat), collapse=", ")))
  }

  backend <- obj$backend
  reg <- backend$reg
  id <- backend$id
  loadResult(reg, id=id)
} # value()

#' @export
error.AsyncTask <- function(obj, ...) {
  if (!finished(obj)) {
    throw(sprintf("%s has not finished yet", class(obj)[1L]))
  }

  stat <- status(obj)
  if (!"error" %in% stat) return(NULL)

  backend <- obj$backend
  reg <- backend$reg
  id <- backend$id
  msg <- getErrorMessages(reg, ids=id)
  msg <- paste(sQuote(msg), collapse=", ")
  msg
} # error()


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
#' @param obj The asynchronously task
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
await.AsyncTask <- function(obj, cleanup=TRUE, maxTries=getOption("async::maxTries", 10L), interval=getOption("async::interval", 1.0), ...) {
  throw <- R.methodsS3::throw

  debug <- getOption("async::debug", FALSE)
  if (debug) mprintf("Polling...")

  expr <- obj$expr
  backend <- obj$backend
  reg <- backend$reg
  id <- backend$id

  finished <- FALSE
  stat <- NULL
  tries <- 1L
  while (tries <= maxTries) {
    stat <- status(obj)
    if (any(c("done", "error", "expired") %in% stat)) {
      finished <- TRUE
      break
    }
    if (debug) { mprintf("\n"); mprint(stat) }
    Sys.sleep(interval)
    tries <- tries + 1L
  }

  res <- NULL
  if (finished) {
    if (debug) { mprint("Results:"); mstr(res) }
    if ("done" %in% stat) {
      res <- value(obj)
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      throw("BatchJobError: ", error(obj))
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- "<expired>"
      throw("BatchJobExpiration: ", msg)
    }
  } else {
    throw(sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchroneous evaluation is still running: %s", tries, interval, reg))
  }

  ## Cleanup?
  if (cleanup) removeRegistry(reg, ask = "no")

  res
} # await()


`&.AsyncTask` <- function(x, y) {
  if (inherits(x, "AsyncTask")) x <- await(x)
  if (inherits(y, "AsyncTask")) y <- await(y)
  x & y
}
