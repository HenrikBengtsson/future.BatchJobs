#' Create an asynchroneous task
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which global environment
#' should be located.
#' @param finalize If TRUE, any underlying registries are
#' deleted when this object is garbage collected, otherwise not.
#'
#' @return An AsyncTask object
#'
#' @aliases async
#' @export
#' @export async
#' @importFrom R.utils mcat mstr mprint mprintf
#' @importFrom BatchJobs batchMap submitJobs
#' @keywords internal
AsyncTask <- function(expr=NULL, envir=parent.frame(), finalize=getOption("async::finalize", TRUE)) {
  # Argument 'expr':
  expr <- substitute(expr)

  # Argument 'envir':
  if (!is.environment(envir))
    stop("Argument 'envir' is not an environment: ", class(envir)[1L])


  debug <- getOption("async::debug", FALSE)

  if (!debug) {
    options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")
  }

  if (debug) { mcat("Expression:\n"); mprint(expr) }

##  ## Inject loading of 'async' in case of nested asynchroneous evaluation
##  expr <- substitute({
##    R.utils::use("async")
##    a
##  }, list(a=expr))
##  if (debug) { mcat("Expression (injected):\n"); mprint(expr) }

  ## Create temporary registry
  reg <- tempRegistry()
  if (debug) mprint(reg)

  ## Create job
  id <- asyncBatchEvalQ(reg, exprs=list(expr), globals=TRUE, envir=envir)
  if (debug) mprintf("Created job #%d\n", id)

  ## Setup return value
  obj <- list(
    expr=expr,
    envir=envir,
    backend=list(reg=reg, id=id)
  )
  obj <- structure(obj, class=c("AsyncTask"))

  ## Register finalizer (will clean up registries
  if (finalize) {
    ## Use a "dummy" environment for GC finalization
    gcenv <- new.env()
    gcenv$obj <- obj

    reg.finalizer(gcenv, f=function(gcenv) {
      obj <- gcenv$obj
      gcenv$obj <- NULL
      if (inherits(obj, "AsyncTask") && "async" %in% loadedNamespaces()) {
        try( delete(obj, onFailure="warning", onMissing="ignore") )
      }
    }, onexit=TRUE)

    obj$.gcenv <- gcenv
    gcenv <- NULL
  }

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
#' @keywords internal
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
#' @importFrom BatchJobs getStatus
#' @keywords internal
status.AsyncTask <- function(obj, ...) {
  backend <- obj$backend
  reg <- backend$reg
  if (!inherits(reg, "Registry")) return(NA_character_)

  id <- backend$id
  status <- getStatus(reg, ids=id)
  status <- (unlist(status) == 1L)
  status <- status[status]
  status <- sort(names(status))
  status <- setdiff(status, c("n"))
  status
} # status()


#' @export
#' @keywords internal
finished.AsyncTask <- function(obj, ...) {
  any(c("done", "error", "expired") %in% status(obj))
}


#' @export
#' @keywords internal
value.AsyncTask <- function(obj, ...) {
  stat <- status(obj)
  if (!"done" %in% stat) {
    throw(AsyncTaskError(sprintf("%s did not succeed: %s", class(obj)[1L], paste(sQuote(stat), collapse=", ")), task=obj))
  }

  backend <- obj$backend
  reg <- backend$reg
  id <- backend$id
  loadResult(reg, id=id)
} # value()

#' @export
#' @keywords internal
error.AsyncTask <- function(obj, ...) {
  if (!finished(obj)) {
    throw(AsyncTaskError(sprintf("%s has not finished yet", class(obj)[1L]), task=obj))
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
#' @keywords internal
await <- function(...) UseMethod("await")


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
#' @param obj The asynchronously task
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
#' @importFrom R.utils mprint mprintf mstr
#' @importFrom BatchJobs getErrorMessages loadResult removeRegistry
#' @keywords internal
await.AsyncTask <- function(obj, cleanup=TRUE, maxTries=getOption("async::maxTries", Sys.getenv("R_ASYNC_MAXTRIES", 1000)), delta=getOption("async::interval", 1.0), alpha=1.01, ...) {
  throw <- R.methodsS3::throw

  maxTries <- as.integer(maxTries)

  debug <- getOption("async::debug", FALSE)
  if (debug) mprintf("Polling...\n")

  expr <- obj$expr
  backend <- obj$backend
  reg <- backend$reg
  id <- backend$id

  ## It appears that occassionally a job can shown as 'expired'
  ## just before becoming 'done'.  It's odd and should be reported
  ## but here's a workaround that won't trust 'expired' without
  ## confirming it several times with delay.
  final_countdown <- 5L
  final_state <- NULL
  final_state_prev <- NULL
  finish_states <- c("done", "error", "expired")

  stat <- NULL
  tries <- 1L
  interval <- delta
  finished <- FALSE
  while (tries <= maxTries) {
    stat <- status(obj)
    if (debug) mprintf(" Status %d: %s\n", tries, paste(stat, collapse=", "))
    if (any(finish_states %in% stat)) {
      final_state <- intersect(stat, finish_states)

      ## ROBUSTNESS: Don't trust a single 'expired' status.
      ## Need to see that several times before believing it.
      ## See BatchJobs Issue #70.
      if ("expired" %in% final_state) {
        if (!identical(final_state, final_state_prev)) {
          final_state_prev <- final_state
    	  final_countdown <- 5L
          interval <- delta
  	  maxTries <- maxTries + final_countdown
        } else {
          final_countdown <- final_countdown - 1L
          if (debug) mprintf(" 'expired' status countdown: %d\n", final_countdown)
          if (final_countdown == 0L) {
            finished <- TRUE
            break
  	  }
        }
      } else {
        finished <- TRUE
        break
      }
    }
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
      msg <- sprintf("BatchJobError: %s", error(obj))
      throw(AsyncTaskError(msg, task=obj))
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobExpiration: Job of registry '%s' expired: %s", reg$id, reg$file.dir)
      throw(AsyncTaskError(msg, task=obj))
    }
  } else {
    cleanup <- FALSE
    msg <- sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchroneous evaluation is still running: BatchJobs registry '%s' (%s)", tries-1L, interval, reg$id, reg$file.dir)
    throw(AsyncTaskError(msg, task=obj))
  }

  ## Cleanup?
  if (cleanup) {
    delete(obj, delta=0.5*delta, alpha=alpha, ...)
  }

  res
} # await()


#' Removes an asynchroneous task
#'
#' @param obj The asynchronously task
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
delete.AsyncTask <- function(obj, onFailure=c("error", "warning", "ignore"), onMissing=c("ignore", "warning", "error"), maxTries=10L, delta=getOption("async::interval", 1.0), alpha=1.01, ...) {
  onMissing <- match.arg(onMissing)
  onFailure <- match.arg(onFailure)

  ## Identify registry
  backend <- obj$backend
  reg <- backend$reg
  path <- reg$file.dir

  ## Already deleted?
  if (is.null(path) || !file_test("-d", path)) {
    if (onMissing %in% c("warning", "error")) {
      msg <- sprintf("Cannot remove BatchJob registry, because directory does not exist: %s", sQuote(path))
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        throw(AsyncTaskError(msg, task=obj))
      }
    }
    return(invisible(TRUE))
  }


  ## Try to delete registry
  interval <- delta
  for (kk in seq_len(maxTries)) {
    try(removeRegistry(reg, ask="no"), silent=TRUE)
    if (!file_test("-d", path)) break
    Sys.sleep(interval)
    interval <- alpha*interval
  }


  ## Sucess?
  if (file_test("-d", path)) {
    if (onFailure %in% c("warning", "error")) {
      msg <- sprintf("Failed to remove BatchJob registry: %s", sQuote(path))
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        throw(AsyncTaskError(msg, task=obj))
      }
    }
    return(invisible(FALSE))
  }

  invisible(TRUE)
} # delete()

delete <- function(...) UseMethod("delete")


#' Inspect an asynchroneous variable
#'
#' @param var the variable.
#' @param envir the environment where to search from.
#' @param inherits Search parent frames or not.
#'
#' @return An AsyncTask object
#'
#' @export
inspect <- function(var, envir=parent.frame(), inherits=TRUE) {
  expr <- substitute(var)
  if (is.language(expr)) {
    n <- length(expr)
    name <- paste(deparse(expr), collapse="")
    if (n != 1L && n != 3L) {
      stop("Not a valid variable format: ", name, call.=FALSE)
    }
    if (n == 1L) {
      ## Assignment to a variable name
      if (!grepl("^[.a-zA-Z]", name)) {
        stop("Not a valid variable name: ", name, call.=FALSE)
      }
    } else if (n == 3L) {
      ## Assignment to enviroment via $ and [[
      op <- expr[[1]]
      if (op == "$" || op == "[[") {
        ## Target
        objname <- deparse(expr[[2]])
        if (!exists(objname, envir=envir, inherits=TRUE)) {
          stop(sprintf("Object %s not found: %s", sQuote(objname), name), call.=FALSE)
        }
        obj <- get(objname, envir=envir, inherits=TRUE)

        ## Subset
        idx <- expr[[3]]
        if (is.symbol(idx)) {
          idx <- deparse(idx)
          if (op == "[[") {
            if (!exists(idx, envir=envir, inherits=TRUE)) {
              stop(sprintf("Object %s not found: %s", sQuote(idx), name), call.=FALSE)
            }
            idx <- get(idx, envir=envir, inherits=TRUE)
          }
        } else if (is.language(idx)) {
          idx <- eval(idx, envir=envir)
        }

        ## Validate subetting, i.e. the 'idx'
        if (length(idx) > 1L) {
          stop(sprintf("Inspection can only be done on a single element at the time, not %d: %s", length(idx), name), call.=FALSE)
        }

        ## Special: listenv:s
        if (inherits(obj, "listenv")) {
          ## Get variable name to use
          idx <- get_variable(obj, idx)
        }

        if (is.character(idx)) {
        } else if (is.numeric(idx)) {
          stop(sprintf("Inspection with indexed subsetting can not be done on a %s: %s", sQuote(mode(obj)), name), call.=FALSE)
        } else {
          stop(sprintf("Invalid subset %s: %s", sQuote(deparse(idx)), name), call.=FALSE)
        }

        if (is.environment(obj)) {
          name <- idx
          envir <- obj
        } else {
          stop(sprintf("Inspection can not be done to a %s; only to a variable or an environment: %s", sQuote(mode(obj)), name), call.=FALSE)
        }
      }
    } # if (n == 3)
  }

  name <- sprintf(".task_%s", name)
  get(name, mode="list", envir=envir, inherits=inherits)
}
