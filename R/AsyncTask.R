#' Create an asynchroneous task
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which global environment
#' should be located.
#' @param substitute Controls whether \code{expr} should be
#' \code{substitute()}:d or not.
#'
#' @return An AsyncTask object
#'
#' @export
#' @importFrom future Future
#' @keywords internal
AsyncTask <- function(expr=NULL, envir=parent.frame(), substitute=TRUE, ...) {
  if (substitute) expr <- substitute(expr)
  f <- Future(expr=expr, envir=envir)
  structure(f, class=c("AsyncTask", class(f)))
}


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# AsyncTask specific
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
add_finalizer <- function(...) UseMethod("add_finalizer")

add_finalizer.AsyncTask <- function(task, ...) {
  ## Register finalizer (will clean up registries etc.)

  reg.finalizer(task, f=function(gcenv) {
    if (inherits(task, "AsyncTask") && "async" %in% loadedNamespaces()) {
      try({
        delete(task, onRunning="skip", onMissing="ignore", onFailure="warning")
      })
    }
  }, onexit=TRUE)

  invisible(task)
}


#' Print an AsyncTask
#'
#' @param x An AsyncTask object
#' @param \ldots Not used.
#'
#' @export
#' @keywords internal
print.AsyncTask <- function(x, ...) {
  printf("%s:\n", class(x)[1L])
  printf("Expression:\n")
  code <- captureOutput(print(x$expr))
  code <- paste(sprintf("  %s", code), collapse="\n")
  printf("%s\n", code)
  ## Ask for status once
  status <- status(x)
  printf("Status: %s\n", paste(sQuote(status), collapse=", "))
  if ("error" %in% status) printf("Error: %s\n", error(x))
}

#' @importFrom future resolved
#' @export
#' @keywords internal
resolved.AsyncTask <- function(x, ...) {
  resolved <- finished(x)
  if (is.na(resolved)) return(FALSE)
  resolved
}

#' Status of an AsyncTask
#'
#' @param \ldots Arguments passed to the S3 method
#'
#' @return A logical or aA character vector.
#'
#' @aliases finished completed failed expired value error
#' @export
#' @export finished
#' @export completed
#' @export failed
#' @export expired
#' @export error
#' @keywords internal
status <- function(...) UseMethod("status")
finished <- function(...) UseMethod("finished")
completed <- function(...) UseMethod("completed")
failed <- function(...) UseMethod("failed")
expired <- function(...) UseMethod("expired")
error <- function(...) UseMethod("error")


#' @export
#' @keywords internal
status.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}

#' @export
#' @keywords internal
finished.AsyncTask <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  any(c("done", "error", "expired") %in% status)
}


#' @export
#' @keywords internal
completed.AsyncTask <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  ("done" %in% status) && !any(c("error", "expired") %in% status)
}

#' @export
#' @keywords internal
failed.AsyncTask <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  any("error" %in% status)
}

#' @export
#' @keywords internal
expired.AsyncTask <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  any("expired" %in% status)
}


#' @export
#' @keywords internal
error.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}

run <- function(...) UseMethod("run")

#' @export
#' @keywords internal
run.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}


#' Awaits an asynchroneous task
#'
#' @param task The asynchronously task
#' @param cleanup If TRUE, the registry is completely removed upon
#' success, otherwise not.
#' @param maxTries The number of tries before giving up.
#' @param delta The number of seconds to wait the first time.
#' @param alpha A factor to scale up the waiting time in each iteration
#' such that the waiting time in the k:th iteration is \code{alpha^k*delta}.
#' @param \ldots Not used.
#'
#' @return The value of the evaluated expression.
#' If an error occurs, an informative Exception is thrown.
#'
#' #' @details
#' Note that \code{await()} should only be called once, because
#' after being called the actual asynchroneous task may be removed
#' and will no longer available in subsequent calls.  If called
#' again, an error may be thrown.
#'
#' @export
#' @keywords internal
await <- function(...) UseMethod("await")

#' @export
#' @keywords internal
await.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}




#' Removes an asynchroneous task
#'
#' @param task The asynchronously task
#' @param onRunning Action if task is running or appears to run.
#' @param onFailure Action if failing to delete task.
#' @param onMissing Action if task does not exist.
#' @param maxTries The number of tries before giving up.
#' @param delta The delay interval (in seconds) between retries.
#' @param alpha A multiplicative penalty increasing the delay
#' for each failed try.
#' @param \ldots Not used.
#'
#' @return (invisibly) TRUE if deleted and FALSE otherwise.
#'
#' @export
#' @aliases delete.AsyncTask
#' @keywords internal
delete <- function(...) UseMethod("delete")

#' @export
delete.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}
