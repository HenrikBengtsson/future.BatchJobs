#' Create an asynchroneous task
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which global environment
#' should be located.
#' @param substitute Controls whether \code{expr} should be
#' \code{substitute()}:d or not.
#' @param finalize If TRUE, any underlying registries are
#' deleted when this object is garbage collected, otherwise not.
#'
#' @return An AsyncTask object
#'
#' @export
#' @importFrom R.utils mcat mstr mprint mprintf
#' @importFrom BatchJobs submitJobs
#' @keywords internal
BatchJobsAsyncTask <- function(expr=NULL, envir=parent.frame(), substitute=TRUE, finalize=getOption("async::finalize", TRUE), launch=TRUE, ...) {
  if (substitute) expr <- substitute(expr)

  ## 1. Setup task
  task <- AsyncTask(expr=expr, envir=envir, substitute=FALSE, ...)

  debug <- getOption("async::debug", FALSE)
  if (!debug) options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")

  ## 2. Add backend to task
  reg <- tempRegistry()
  if (debug) mprint(reg)
  id <- asyncBatchEvalQ(reg, exprs=list(expr), globals=TRUE, envir=envir)
  task$backend <- list(reg=reg, id=id)
  task <- structure(task, class=c("BatchJobsAsyncTask", class(task)))
  if (debug) mprintf("Created task #%d\n", id)

  ## Register finalizer?
  if (finalize) task <- add_finalizer(task)

  ## 3. Launch task?
  if (launch) {
    submitJobs(reg, ids=id)
    if (debug) mprintf("Launched task #%d\n", id)
  }

  task
}


#' Print an AsyncTask
#'
#' @param x An AsyncTask object
#' @param ... Not used.
#'
#' @export
#' @importFrom R.utils printf
#' @keywords internal
print.BatchJobsAsyncTask <- function(x, ...) {
  NextMethod("print")
  printf("Backend:\n")
  backend <- x$backend
  reg <- backend$reg
  if (isNA(status(x))) {
    printf("%s: Not found (happens when finished and deleted)\n", class(reg))
  } else {
    print(reg)
  }
}


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
status.BatchJobsAsyncTask <- function(task, ...) {
  backend <- task$backend
  reg <- backend$reg
  if (!inherits(reg, "Registry")) return(NA_character_)
  ## Closed and deleted?
  if (!file_test("-d", reg$file.dir)) return(NA_character_)

  id <- backend$id
  status <- getStatus(reg, ids=id)
  status <- (unlist(status) == 1L)
  status <- status[status]
  status <- sort(names(status))
  status <- setdiff(status, c("n"))
  status
} # status()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Future API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @export
#' @keywords internal
value.BatchJobsAsyncTask <- function(task, onCondition=c("signal", "return"), onMissing=c("default", "error"), default=NULL, ...) {
  onCondition <- match.arg(onCondition)
  onMissing <- match.arg(onMissing)

  stat <- status(task)
  if (isNA(stat)) {
    if (onMissing == "default") return(default)
    stop(sprintf("The value no longer exists (or never existed) for Future of class ", paste(sQuote(class(task)), collapse=", ")))
  }

  if (!"done" %in% stat) {
    throw(AsyncTaskError(sprintf("%s did not succeed: %s", class(task)[1L], paste(sQuote(stat), collapse=", ")), task=task))
  }

  backend <- task$backend
  reg <- backend$reg
  id <- backend$id
  loadResult(reg, id=id)
} # value()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# AsyncTask API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @export
#' @keywords internal
error.BatchJobsAsyncTask <- function(task, ...) {
  stat <- status(task)
  if (isNA(stat)) return(NULL)

  if (!finished(task)) {
    throw(AsyncTaskError(sprintf("%s has not finished yet", class(task)[1L]), task=task))
  }

  if (!"error" %in% stat) return(NULL)

  backend <- task$backend
  reg <- backend$reg
  id <- backend$id
  msg <- getErrorMessages(reg, ids=id)
  msg <- paste(sQuote(msg), collapse=", ")
  msg
} # error()


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
#' @importFrom R.utils mprint mprintf mstr
#' @importFrom BatchJobs getErrorMessages loadResult removeRegistry
#' @keywords internal
await.BatchJobsAsyncTask <- function(task, cleanup=TRUE, maxTries=getOption("async::maxTries", Sys.getenv("R_ASYNC_MAXTRIES", 1000)), delta=getOption("async::interval", 1.0), alpha=1.01, ...) {
  throw <- R.methodsS3::throw

  maxTries <- as.integer(maxTries)

  debug <- getOption("async::debug", FALSE)
  if (debug) mprintf("Polling...\n")

  expr <- task$expr
  backend <- task$backend
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
    stat <- status(task)
    if (debug) mprintf(" Status %d: %s\n", tries, paste(stat, collapse=", "))
    if (isNA(stat)) {
      finished <- TRUE
      break
    }
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
      res <- value(task)
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobError: %s", error(task))
      throw(AsyncTaskError(msg, task=task))
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobExpiration: Job of registry '%s' expired: %s", reg$id, reg$file.dir)
      throw(AsyncTaskError(msg, task=task))
    } else if (isNA(stat)) {
      msg <- sprintf("BatchJobDeleted: Cannot retrieve value. Job of registry '%s' deleted: %s", reg$id, reg$file.dir)
      throw(AsyncTaskError(msg, task=task))
    }
  } else {
    cleanup <- FALSE
    msg <- sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchroneous evaluation is still running: BatchJobs registry '%s' (%s)", tries-1L, interval, reg$id, reg$file.dir)
    throw(AsyncTaskError(msg, task=task))
  }

  ## Cleanup?
  if (cleanup) {
    delete(task, delta=0.5*delta, alpha=alpha, ...)
  }

  res
} # await()


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
#' @param ... Not used.
#'
#' @return (invisibly) TRUE if deleted and FALSE otherwise.
#'
#' @export
#' @importFrom BatchJobs removeRegistry
#' @keywords internal
delete.BatchJobsAsyncTask <- function(task, onRunning=c("warning", "error", "skip"), onFailure=c("error", "warning", "ignore"), onMissing=c("ignore", "warning", "error"), maxTries=10L, delta=getOption("async::interval", 1.0), alpha=1.01, ...) {
  onRunning <- match.arg(onRunning)
  onMissing <- match.arg(onMissing)
  onFailure <- match.arg(onFailure)

  ## Identify registry
  backend <- task$backend
  reg <- backend$reg
  path <- reg$file.dir

  ## Already deleted?
  if (is.null(path) || !file_test("-d", path)) {
    if (onMissing %in% c("warning", "error")) {
      msg <- sprintf("Cannot remove BatchJob registry, because directory does not exist: %s", sQuote(path))
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        throw(AsyncTaskError(msg, task=task))
      }
    }
    return(invisible(TRUE))
  }


  ## Does the task still run?  If so, then...
  isRunning <- FALSE ## FIX ME: Code here!
  if (isRunning) {
    if (onRunning == "skip") return(invisible(TRUE))

    msg <- sprintf("Will not remove BatchJob registry, because is appears to hold a running task: %s", sQuote(path))
    if (onRunning == "warning") {
      warning(msg)
      return(invisible(TRUE))
    } else if (onRunning == "error") {
      throw(AsyncTaskError(msg, task=task))
    }
  }


  ## Try to delete registry
  interval <- delta
  for (kk in seq_len(maxTries)) {
    try(removeRegistry(reg, ask="no"), silent=TRUE)
    if (!file_test("-d", path)) break
    Sys.sleep(interval)
    interval <- alpha*interval
  }


  ## Success?
  if (file_test("-d", path)) {
    if (onFailure %in% c("warning", "error")) {
      msg <- sprintf("Failed to remove BatchJob registry: %s", sQuote(path))
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        throw(AsyncTaskError(msg, task=task))
      }
    }
    return(invisible(FALSE))
  }

  invisible(TRUE)
} # delete()
