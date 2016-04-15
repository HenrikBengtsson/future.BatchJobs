#' A BatchJobs future is a future whose value will be resolved via BatchJobs
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which global environment
#' should be located.
#' @param substitute Controls whether \code{expr} should be
#' \code{substitute()}:d or not.
#' @param backend The BatchJobs backend to use, cf. \code{\link{backend}()}.
#' @param resources A named list of resources needed by this future.
#' @param finalize If TRUE, any underlying registries are
#' deleted when this object is garbage collected, otherwise not.
#' @param \ldots Additional arguments pass to \code{\link[future]{Future}()}.
#'
#' @return A BatchJobsFuture object
#'
#' @export
#' @importFrom future Future
#' @importFrom BatchJobs submitJobs
#' @keywords internal
BatchJobsFuture <- function(expr=NULL, envir=parent.frame(), substitute=TRUE, backend=NULL, resources=list(), finalize=getOption("future.finalize", TRUE), ...) {
  if (substitute) expr <- substitute(expr)
  stopifnot(is.list(resources),
            length(resources) == 0 || !is.null(names(resources)))

  debug <- getOption("future.debug", FALSE)
  if (!debug) options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")

  ## Record globals
  getGlobalsAndPackages <- importFuture("getGlobalsAndPackages")
  gp <- getGlobalsAndPackages(expr, envir=envir)

  ## 1. Create BatchJobs registry
  reg <- tempRegistry()
  if (debug) mprint(reg)

  ## 2. Create BatchJobsFuture object
  future <- Future(expr=gp$expr, envir=envir, substitute=FALSE, ...)

  future$globals <- gp$globals
  future$packages <- gp$packages
  future$config <- list(reg=reg, id=NA_integer_,
                      cluster.functions=NULL,
                      resources=resources,
                      backend=backend)

  future <- structure(future, class=c("BatchJobsFuture", class(future)))


  ## Register finalizer?
  if (finalize) future <- add_finalizer(future)

  future
}


#' Prints a BatchJobs future
#'
#' @param x An BatchJobsFuture object
#' @param \ldots Not used.
#'
#' @export
#' @keywords internal
print.BatchJobsFuture <- function(x, ...) {
  printf("%s:\n", class(x)[1L])

  printf("Expression:\n")
  code <- captureOutput(print(x$expr))
  code <- paste(sprintf("  %s", code), collapse="\n")
  printf("%s\n", code)

  ## Ask for status once
  status <- status(x)
  printf("Status: %s\n", paste(sQuote(status), collapse=", "))
  if ("error" %in% status) printf("Error: %s\n", error(x))

  printf("BatchJobs configuration:\n")
  config <- x$config
  reg <- config$reg
  if (isNA(status(x))) {
    printf("%s: Not found (happens when finished and deleted)\n", class(reg))
  } else {
    print(reg)
    printf("Cluster functions: %s\n", sQuote(config$cluster.functions$name))
  }
}


status <- function(...) UseMethod("status")
finished <- function(...) UseMethod("finished")
completed <- function(...) UseMethod("completed")
failed <- function(...) UseMethod("failed")
expired <- function(...) UseMethod("expired")
error <- function(...) UseMethod("error")

#' Status of BatchJobs future
#'
#' @param future The future.
#' @param \ldots Not used.
#'
#' @return A character vector or a logical scalar.
#'
#' @aliases status finished completed failed expired value error
#' @keywords internal
#'
#' @export
#' @export status
#' @export finished
#' @export completed
#' @export failed
#' @export expired
#' @export value
#' @export error
#' @importFrom BatchJobs getStatus
status.BatchJobsFuture <- function(future, ...) {
  config <- future$config
  reg <- config$reg
  if (!inherits(reg, "Registry")) return(NA_character_)
  ## Closed and deleted?
  if (!file_test("-d", reg$file.dir)) return(NA_character_)

  id <- config$id
  if (is.na(id)) return("not submitted")
  status <- getStatus(reg, ids=id)
  status <- (unlist(status) == 1L)
  status <- status[status]
  status <- sort(names(status))
  status <- setdiff(status, c("n"))
  status
} # status()



#' @export
#' @keywords internal
finished.BatchJobsFuture <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  any(c("done", "error", "expired") %in% status)
}


#' @export
#' @keywords internal
completed.BatchJobsFuture <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  ("done" %in% status) && !any(c("error", "expired") %in% status)
}

#' @export
#' @keywords internal
failed.BatchJobsFuture <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  any("error" %in% status)
}

#' @export
#' @keywords internal
expired.BatchJobsFuture <- function(task, ...) {
  status <- status(task)
  if (isNA(status)) return(NA)
  any("expired" %in% status)
}

#' @export
#' @keywords internal
error.BatchJobsFuture <- function(future, ...) {
  stat <- status(future)
  if (isNA(stat)) return(NULL)

  if (!finished(future)) {
    msg <- sprintf("%s has not finished yet", class(future)[1L])
    ex <- AsyncTaskError(msg, task=future)
    throw(ex)
  }

  if (!"error" %in% stat) return(NULL)

  config <- future$config
  reg <- config$reg
  id <- config$id
  msg <- getErrorMessages(reg, ids=id)
  msg <- paste(sQuote(msg), collapse=", ")
  msg
} # error()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Future API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom future resolved
#' @export
#' @keywords internal
resolved.BatchJobsFuture <- function(x, ...) {
  resolved <- finished(x)
  if (is.na(resolved)) return(FALSE)
  resolved
}

#' @importFrom future value
#' @export
#' @keywords internal
value.BatchJobsFuture <- function(future, signal=TRUE, onMissing=c("default", "error"), default=NULL, cleanup=TRUE, ...) {
  ## Has the value already been collected?
  if (future$state %in% c('finished', 'failed', 'interrupted')) {
    return(NextMethod("value"))
  }

  stat <- status(future)
  if (isNA(stat)) {
    onMissing <- match.arg(onMissing)
    if (onMissing == "default") return(default)
    stop(sprintf("The value no longer exists (or never existed) for Future of class ", paste(sQuote(class(future)), collapse=", ")))
  }

  tryCatch({
    future$value <- await(future, cleanup=FALSE)
    future$state <- 'finished'
    if (cleanup) delete(future, ...)
  }, simpleError = function(ex) {
    future$state <- 'failed'
    future$value <- ex
  })

  NextMethod("value")
} # value()



run <- function(...) UseMethod("run")

## @importFrom future getExpression
#' @importFrom BatchJobs addRegistryPackages batchExport batchMap
run.BatchJobsFuture <- function(future, ...) {
  mdebug <- importFuture("mdebug")

  getClusterFunctions <- function() {
    ns <- getNamespace("BatchJobs")
    getBatchJobsConf <- get("getBatchJobsConf", envir=ns, mode="function")
    getClusterFunctions <- get("getClusterFunctions", envir=ns, mode="function")
    getClusterFunctions(getBatchJobsConf())
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner <- importFuture("assertOwner")
  assertOwner(future)

  debug <- getOption("future.debug", FALSE)
  if (!debug) options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")

  getExpression <- importFuture("getExpression", default=function(future) future$expr)
  expr <- getExpression(future)

  ## Always evaluate in local environment
  expr <- substitute(local(expr), list(expr=expr))

  reg <- future$config$reg
  stopifnot(inherits(reg, "Registry"))

  resources <- future$config$resources

  ## (ii) Attach packages that needs to be attached
  packages <- future$packages
  if (length(packages) > 0) {
    mdebug("Attaching %d packages (%s) ...",
                    length(packages), hpaste(sQuote(packages)))

    ## Record which packages in 'pkgs' that are loaded and
    ## which of them are attached (at this point in time).
    isLoaded <- is.element(packages, loadedNamespaces())
    isAttached <- is.element(packages, attachedPackages())

    ## FIXME: Update the expression such that the new session
    ## will have the same state of (loaded, attached) packages.

    addRegistryPackages(reg, packages=packages)

    mdebug("Attaching %d packages (%s) ... DONE",
                    length(packages), hpaste(sQuote(packages)))
  }
  ## Not needed anymore
  packages <- NULL

  ## (iii) Export globals
  globals <- future$globals
  if (length(globals) > 0) {
    ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    ## Any globals to encode/decore to workaround various
    ## BatchJobs limitations.
    ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
    globalsToEncode <- NULL

    ## BatchJobs::batchExport() validated names of globals using
    ## checkmate::assertList(more.args, names="strict") which doesn't
    ## like names such as "{", although they should be valid indeed.
    ## Details: https://github.com/tudo-r/BatchJobs/issues/93
    keep <- grepl("^[.a-zA-Z]", names(globals))
    if (!all(keep)) {
      names <- names(globals)[!keep]
      globalsToEncode <- c(globalsToEncode, names)
      msg <- sprintf("WORKAROUND: BatchJobs does not support exporting of variables with names that does not match pattern '[a-zA-Z0-9._-]+' (see https://github.com/tudo-r/BatchJobs/issues/93). Encoding/decoding the following global variables: %s", hpaste(sQuote(names)))
      if (debug) mcat(msg)
    }

    ## FIXME: The below can be removed with
    ##        fail (>= 1.3) and BatchJobs (>= 1.7)
    ##        /HB 2015-10-20
    ## BatchJobs::loadExports() ignores exported variables that
    ## start with a period.
    ## Details: https://github.com/tudo-r/BatchJobs/issues/103
    bad <- grepl("^[.]", names(globals))
    if (any(bad)) {
      names <- names(globals)[bad]
      globalsToEncode <- c(globalsToEncode, names)
      msg <- sprintf("WORKAROUND: BatchJobs does not support exported variables that start with a period (see https://github.com/tudo-r/BatchJobs/issues/103). Encoding/decoding the following global variables: %s", hpaste(sQuote(names)))
      if (debug) mcat(msg)
    }

    ## Does any globals need to be encoded/decoded to workaround
    ## the limitations of BatchJobs?
    if (length(globalsToEncode) > 0L) {
      ## (a) URL encode global variable names
      globalsToDecode <- sapply(globalsToEncode, FUN=utils::URLencode, reserved=TRUE)
      ## (b) Substitute '%' with '_.PERCENT._'
      globalsToDecode <- gsub("%", "_.PERCENT._", globalsToDecode, fixed=TRUE)

      ## (c) Append with 'R_ASYNC_RENAME_'
      globalsToDecode <- paste("R_ASYNC_RENAME_", globalsToDecode, sep="")

      ## (d) Rename corresponding globals
      names <- names(globals)
      idxs <- match(globalsToEncode, names)
      names[idxs] <- globalsToDecode
      names(globals) <- names

      ## (d) Record variables which to be rename by the future
      globals <- append(globals, list(R_ASYNC_GLOBALS_TO_RENAME=globalsToDecode))

      ## (e) Tweak future expression to decode encoded globals
      expr <- substitute({
        ## Decode exported globals (workaround for BatchJobs)
        for (..key.. in R_ASYNC_GLOBALS_TO_RENAME) {
          ..key2.. <- sub("^R_ASYNC_RENAME_", "", ..key..)
          ..key2.. <- gsub("_.PERCENT._", "%", ..key2.., fixed=TRUE)
          ..key2.. <- utils::URLdecode(..key2..)
          assign(..key2.., get(..key.., inherits=FALSE), inherits=FALSE)
        }
        rm(list=c("..key..", "..key2..", "R_ASYNC_GLOBALS_TO_RENAME"))

        expr
      }, list(expr=expr))
    }
    ## Not needed anymore
    globalsToDecode <- NULL

    ## Export via BatchJobs
    batchExport(reg, li=globals)
  }
  ## Not needed anymore
  globals <- NULL

  ## 1. Add to BatchJobs for evaluation
  id <- batchMap(reg, fun=geval, list(expr), more.args=list(substitute=TRUE))

  ## 2. Update
  future$config$id <- id
  if (debug) mprintf("Created %s future #%d\n", class(future)[1], id)

  ## 3. Set backend here
  ## Use a non-default backend?
  backend <- future$config$backend ### FIXME: /HB 2016-03-20 Issue #49
  if (!is.null(backend)) {
    obackend <- backend()
    on.exit(backend(obackend))
    backend(backend)

    ## FIXME: This one too here?!?  /HB 2016-03-20 Issue #49
    ## If/when makeRegistry() attaches BatchJobs, we need
    ## to prevent it from overriding the configuration
    ## already set by backend().
    oopts <- options(BatchJobs.load.config=FALSE)
    on.exit(options(oopts), add=TRUE)
  }

  ## 4. Record
  future$config$cluster.functions <- getClusterFunctions()

  ## 5. Submit
  future$state <- 'running'
  submitJobs(reg, ids=id, resources=resources)
  if (debug) mprintf("Launched future #%d\n", id)

  invisible(future)
}


await <- function(...) UseMethod("await")

#' Awaits the value of a BatchJobs future
#'
#' @param future The future.
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
#' @details
#' Note that \code{await()} should only be called once, because
#' after being called the actual asynchroneous task may be removed
#' and will no longer available in subsequent calls.  If called
#' again, an error may be thrown.
#'
#' @export
#' @importFrom R.methodsS3 throw
#' @importFrom BatchJobs getErrorMessages loadResult removeRegistry
#' @keywords internal
await.BatchJobsFuture <- function(future, cleanup=TRUE, maxTries=getOption("future.maxTries", Sys.getenv("R_FUTURE_MAXTRIES", 1000)), delta=getOption("future.interval", 1.0), alpha=1.01, ...) {
  throw <- R.methodsS3::throw

  maxTries <- as.integer(maxTries)

  debug <- getOption("future.debug", FALSE)
  if (debug) mprintf("Polling...\n")

  expr <- future$expr
  config <- future$config
  reg <- config$reg
  id <- config$id

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
    stat <- status(future)
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
    if (debug) { mprint("Results:") }
    if ("done" %in% stat) {
      res <- loadResult(reg, id=id)
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobError: %s", error(future))
      ex <- AsyncTaskError(msg, task=future)
      throw(ex)
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobExpiration: Job of registry '%s' expired: %s", reg$id, reg$file.dir)
      ex <- AsyncTaskError(msg, task=future)
      throw(ex)
    } else if (isNA(stat)) {
      msg <- sprintf("BatchJobDeleted: Cannot retrieve value. Job of registry '%s' deleted: %s", reg$id, reg$file.dir)
      ex <- AsyncTaskError(msg, task=future)
      throw(ex)
    }
    if (debug) { mstr(res) }
  } else {
    cleanup <- FALSE
    msg <- sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchroneous evaluation is still running: BatchJobs registry '%s' (%s)", tries-1L, interval, reg$id, reg$file.dir)
    ex <- AsyncTaskError(msg, task=future)
    throw(ex)
  }

  ## Cleanup?
  if (cleanup) {
    delete(future, delta=0.5*delta, alpha=alpha, ...)
  }

  res
} # await()


delete <- function(...) UseMethod("delete")

#' Removes a BatchJobs future
#'
#' @param future The future.
#' @param onRunning Action if future is running or appears to run.
#' @param onFailure Action if failing to delete future.
#' @param onMissing Action if future does not exist.
#' @param maxTries The number of tries before giving up.
#' @param delta The delay interval (in seconds) between retries.
#' @param alpha A multiplicative penalty increasing the delay
#' for each failed try.
#' @param \ldots Not used.
#'
#' @return (invisibly) TRUE if deleted and FALSE otherwise.
#'
#' @export
#' @importFrom BatchJobs removeRegistry
#' @keywords internal
delete.BatchJobsFuture <- function(future, onRunning=c("warning", "error", "skip"), onFailure=c("error", "warning", "ignore"), onMissing=c("ignore", "warning", "error"), maxTries=10L, delta=getOption("future.interval", 1.0), alpha=1.01, ...) {
  onRunning <- match.arg(onRunning)
  onMissing <- match.arg(onMissing)
  onFailure <- match.arg(onFailure)

  ## Identify registry
  config <- future$config
  reg <- config$reg
  path <- reg$file.dir

  ## Already deleted?
  if (is.null(path) || !file_test("-d", path)) {
    if (onMissing %in% c("warning", "error")) {
      msg <- sprintf("Cannot remove BatchJob registry, because directory does not exist: %s", sQuote(path))
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        ex <- AsyncTaskError(msg, task=future)
        throw(ex)
      }
    }
    return(invisible(TRUE))
  }


  ## Does the future still run?  If so, then...
  if (future$state == 'running') {
    if (onRunning == "skip") return(invisible(TRUE))

    msg <- sprintf("Will not remove BatchJob registry, because is appears to hold a running future: %s", sQuote(path))
    if (onRunning == "warning") {
      warning(msg)
      return(invisible(TRUE))
    } else if (onRunning == "error") {
      ex <- AsyncTaskError(msg, task=future)
      throw(ex)
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
        ex <- AsyncTaskError(msg, task=future)
        throw(ex)
      }
    }
    return(invisible(FALSE))
  }

  invisible(TRUE)
} # delete()


add_finalizer <- function(...) UseMethod("add_finalizer")

add_finalizer.BatchJobsFuture <- function(future, ...) {
  ## Register finalizer (will clean up registries etc.)

  reg.finalizer(future, f=function(gcenv) {
    if (inherits(future, "BatchJobsFuture") && "async" %in% loadedNamespaces()) {
      try({
        delete(future, onRunning="skip", onMissing="ignore", onFailure="warning")
      })
    }
  }, onexit=TRUE)

  invisible(future)
}
