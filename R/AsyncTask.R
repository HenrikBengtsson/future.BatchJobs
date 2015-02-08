#' @export
AsyncTask <- function(expr=NULL, reg=NA, id=1L, ...) {
  exprT <- substitute(expr)
  obj <- list(
    expr=exprT,
    backend=list(reg=reg, id=id)
  )
  structure(obj, class=c("AsyncTask"))
}

#' @export
#' @importFrom R.utils captureOutput
#' @importFrom R.utils printf
print.AsyncTask <- function(x, ...) {
  printf("%s:\n", class(x)[1])
  printf("Expression:\n")
  code <- captureOutput(print(x$expr))
  code <- paste(sprintf("  %s", code), collapse="\n")
  printf("%s\n", code)
  backend <- x$backend
  printf("Status: %s\n", paste(sQuote(status(x)), collapse=", "))
  printf("Backend:\n")
  print(backend$reg)
}


#' @export
status <- function(...) UseMethod("status")

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
await.AsyncTask <- function(obj, cleanup=FALSE, maxTries=getOption("async::maxTries", 10L), interval=getOption("async::interval", 1.0), ...) {
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
      res <- loadResult(reg, id=id)
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      msg <- getErrorMessages(reg, ids=id)
      msg <- paste(sQuote(msg), collapse=", ")
      throw("BatchJobError: ", msg)
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- "<expired>"
      throw("BatchJobExpiration: ", msg)
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
