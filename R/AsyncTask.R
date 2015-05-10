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
#' @importFrom R.utils mprint
#' @keywords internal
AsyncTask <- function(expr=NULL, envir=parent.frame(), substitute=TRUE, ...) {
  if (substitute) expr <- substitute(expr)

  # Argument 'envir':
  if (!is.environment(envir))
    stop("Argument 'envir' is not an environment: ", class(envir)[1L])


  debug <- getOption("async::debug", FALSE)
  if (!debug) options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")
  if (debug) { mcat("Expression:\n"); mprint(expr) }

##  ## Inject loading of 'async' in case of nested asynchroneous evaluation
##  expr <- substitute({
##    R.utils::use("async")
##    a
##  }, list(a=expr))
##  if (debug) { mcat("Expression (injected):\n"); mprint(expr) }

  ## Setup return value
  task <- list(
    expr=expr,
    envir=envir
  )
  structure(task, class=c("AsyncTask", class(task)))
}

add_finalizer <- function(...) UseMethod("add_finalizer")

add_finalizer.AsyncTask <- function(task, ...) {
  ## Register finalizer (will clean up registries etc.)

  ## Use a "dummy" environment for GC finalization
  gcenv <- new.env()
  gcenv$task <- task

  reg.finalizer(gcenv, f=function(gcenv) {
    task <- gcenv$task
    gcenv$task <- NULL
    if (inherits(task, "AsyncTask") && "async" %in% loadedNamespaces()) {
      try( delete(task, onFailure="warning", onMissing="ignore") )
    }
  }, onexit=TRUE)

  task$.gcenv <- gcenv
  gcenv <- NULL

  invisible(task)
}


#' Print an AsyncTask
#'
#' @param x An AsyncTask object
#' @param ... Not used.
#'
#' @export
#' @importFrom R.utils captureOutput
#' @importFrom R.utils printf
#' @keywords internal
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
  print(backend)
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
#' @keywords internal
status <- function(...) UseMethod("status")
finished <- function(...) UseMethod("finished")
value <- function(...) UseMethod("value")
error <- function(...) UseMethod("error")

#' Status of an AsyncTask
#'
#' @param task The asynchronously task
#' @param ... Not used.
#'
#' @return A character vector.
#'
#' @export
#' @importFrom BatchJobs getStatus
#' @keywords internal
status.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}

#' @export
#' @keywords internal
finished.AsyncTask <- function(task, ...) {
  stat <- status(task)
  if (isNA(stat)) return(NA)

  any(c("done", "error", "expired") %in% stat)
}


#' @export
#' @keywords internal
value.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}

#' @export
#' @keywords internal
error.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}


#' @keywords internal
record <- function(...) UseMethod("record")
record.AsyncTask <- function(task, name) {
  name <- sprintf(".task_%s", name)
  task_without_gc <- task
  task_without_gc$.gcenv <- NULL
  assign(name, task_without_gc, envir=task$envir)
}


#' Retrieves the value of of the asynchronously evaluated expression
#'
#' @param task The asynchronously task
#' @param cleanup If TRUE, the registry is completely removed upon
#' success, otherwise not.
#' @param maxTries The number of tries before giving up.
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
#' @importFrom BatchJobs getErrorMessages loadResult removeRegistry
#' @keywords internal
await.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}


#' Removes an asynchroneous task
#'
#' @param task The asynchronously task
#' @param onFailure Action if failing to delete task.
#' @param onMissing Action if task does not exist.
#' @param maxTries The number of tries before giving up.
#' @param delta The delay interval (in seconds) between retries.
#' @param alpha A multiplicative penalty increasing the delay
#' for each failed try.
#' @param ... Not used.
#'
#' @return (invisibly) TRUE if deleted and FALSE otherwise.
#'
#' @export
#' @export delete
#' @aliases delete
#' @importFrom BatchJobs removeRegistry
#' @keywords internal
delete.AsyncTask <- function(task, ...) {
  stop("Not implemented for class ", class(task)[1])
}

delete <- function(...) UseMethod("delete")
