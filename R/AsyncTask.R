#' Create an asynchroneous task
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which global environment
#' should be located.
#'
#' @return An AsyncTask object
#'
#' @aliases async
#' @export
#' @export async
#' @importFrom R.utils mcat mstr mprint mprintf
#' @importFrom BatchJobs batchMap submitJobs
AsyncTask <- function(expr=NULL, envir=parent.frame()) {
  # Argument 'expr':
  expr <- substitute(expr)

  # Argument 'envir':
  if (!is.environment(envir))
    throw("Argument 'envir' is not a list: ", class(envir)[1L])


  debug <- getOption("async::debug", FALSE)

  if (!debug) {
    options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")
  }

  if (debug) { mcat("Expression:\n"); mprint(expr) }

  ## Inject loading of 'async' in case of nested asynchroneous evaluation
  expr <- substitute({
    R.utils::use("async")
    a
  }, list(a=expr))
  if (debug) { mcat("Expression (injected):\n"); mprint(expr) }

  ## Create temporary registry
  reg <- tempRegistry()
  if (debug) mprint(reg)

  ## Create job
  id <- batchEval(reg, exprs=list(expr), globals=TRUE, envir=envir)
  if (debug) mprintf("Created job #%d\n", id)

  ## Setup return value
  obj <- list(
    expr=expr,
    backend=list(reg=reg, id=id)
  )
  obj <- structure(obj, class=c("AsyncTask"))

  ## Submit job
  submitJobs(reg, ids=id)
  if (debug) mprintf("Submitted job #%d\n", id)

  obj
}

async <- AsyncTask


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
#' @aliases finished value error
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
#' @param delta The number of seconds to wait the first time.
#' @param alpha A factor to scale up the waiting time in each iteration
#' such that the waiting time in the k:th iteration is \code{alpha^k*delta}.
#' @param ... Not used.
#'
#' @return The value of the evaluated expression.
#' If an error occurs, an informative Exception is thrown.
#'
#' @export
#' @importFrom R.methodsS3 throw
#' @importFrom R.utils mprint mprintf mstr
#' @importFrom BatchJobs getStatus getErrorMessages loadResult removeRegistry
await.AsyncTask <- function(obj, cleanup=TRUE, maxTries=getOption("async::maxTries", Sys.getenv("R_ASYNC_MAXTRIES", 1000)), delta=getOption("async::interval", 1.0), alpha=1.01, ...) {
  throw <- R.methodsS3::throw

  maxTries <- as.integer(maxTries)
  
  debug <- getOption("async::debug", FALSE)
  if (debug) mprintf("Polling...")

  expr <- obj$expr
  backend <- obj$backend
  reg <- backend$reg
  id <- backend$id

  finished <- FALSE
  stat <- NULL
  tries <- 1L
  interval <- delta
  while (tries <= maxTries) {
    stat <- status(obj)
    if (any(c("done", "error", "expired") %in% stat)) {
      finished <- TRUE
      break
    }
    if (debug) { mprintf("\n"); mprint(stat) }
    Sys.sleep(interval)
    interval <- alpha*interval
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
    throw(sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchroneous evaluation is still running: BatchJobs registry '%s' (%s)", tries, interval, reg$id, reg$file.dir))
  }

  ## Cleanup?
  if (cleanup) {
    interval <- 0.1*delta
    for (kk in 1:10) {
      if (isTRUE(try(removeRegistry(reg, ask="no"), silent=TRUE))) break
      Sys.sleep(interval)
      interval <- alpha*interval
    }
    if (file_test("-d", reg$file.dir)) {
      warning(sprintf("Failed to remove BatchJobs registry '%s' (%s)", reg$id, reg$file.dir))
    }
  }

  res
} # await()


`&.AsyncTask` <- function(x, y) {
  if (inherits(x, "AsyncTask")) x <- await(x)
  if (inherits(y, "AsyncTask")) y <- await(y)
  x & y
}
