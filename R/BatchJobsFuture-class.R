#' A BatchJobs future is a future whose value will be resolved via BatchJobs
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which global environment
#' should be located.
#' @param substitute Controls whether \code{expr} should be
#' \code{substitute()}:d or not.
#' @param globals (optional) a logical, a character vector, a named list, or a \link[globals]{Globals} object.  If TRUE, globals are identified by code inspection based on \code{expr} and \code{tweak} searching from environment \code{envir}.  If FALSE, no globals are used.  If a character vector, then globals are identified by lookup based their names \code{globals} searching from environment \code{envir}.  If a named list or a Globals object, the globals are used as is.
#' @param label (optional) Label of the future (where applicable, becomes the job name for most job schedulers).
#' @param conf A BatchJobs configuration environment.
#' @param cluster.functions A BatchJobs \link[BatchJobs]{ClusterFunctions} object.
#' @param resources A named list passed to the BatchJobs template (available as variable \code{resources}).
#' @param workers (optional) Additional specification for
#' the BatchJobs backend.
#' @param job.delay (optional) Passed as is to \code{\link[BatchJobs]{submitJobs}()}.
#' @param finalize If TRUE, any underlying registries are
#' deleted when this object is garbage collected, otherwise not.
#' @param \ldots Additional arguments passed to \code{\link[future]{Future}()}.
#'
#' @return A BatchJobsFuture object
#'
#' @export
#' @importFrom future Future
#' @importFrom BatchJobs submitJobs
#' @keywords internal
BatchJobsFuture <- function(expr=NULL, envir=parent.frame(), substitute=TRUE, globals=TRUE, label="BatchJobs", conf=NULL, cluster.functions=NULL, resources=list(), workers=NULL, job.delay=FALSE, finalize=getOption("future.finalize", TRUE), ...) {
  if (substitute) expr <- substitute(expr)

  if (!is.null(label)) label <- as.character(label)
  
  if (!is.null(conf)) {
    stopifnot(is.environment(conf))
  }

  if (!is.null(cluster.functions)) {
    stopifnot(is.list(cluster.functions))
    conf$cluster.functions <- cluster.functions
  }

  if (!is.null(workers)) {
    stopifnot(length(workers) >= 1)
    if (is.numeric(workers)) {
      stopifnot(!anyNA(workers), all(workers >= 1))
    } else if (is.character(workers)) {
    } else {
      stopifnot("Argument 'workers' should be either numeric or character: ", mode(workers))
    }
  }

  stopifnot(is.list(resources))

  stopifnot(is.logical(job.delay) || is.function(job.delay))

  ## Record globals
  getGlobalsAndPackages <- importFuture("getGlobalsAndPackages")
  gp <- getGlobalsAndPackages(expr, envir=envir, globals=globals)

  ## Create BatchJobsFuture object
  future <- Future(expr=gp$expr, envir=envir, substitute=FALSE, workers=workers, label=label, ...)

  ## LEGACY: /HB 2016-05-20
  backend <- future$backend
  future$backend <- NULL

  future$globals <- gp$globals
  future$packages <- gp$packages
  future$conf <- conf

  ## Create BatchJobs registry
  reg <- tempRegistry(label=future$label)
  debug <- getOption("future.debug", FALSE)
  if (debug) mprint(reg)

  ## BatchJobs configuration
  config <- list(reg=reg, jobid=NA_integer_,
                 cluster.functions=cluster.functions,
                 resources=resources,
                 job.delay=job.delay,
                 backend=backend)


  ## Additional arguments to be available for the BatchJobs template?
  ## NOTE: Support for 'args' will be removed soon. /HB 2016-07-15
  if (is.element("args", names(future))) {
    args <- future$args
    future$args <- NULL ## Cleanup
    
    stopifnot(is.list(args))
    if (length(args) > 0) {
      names <- names(args)
      unknown <- setdiff(names, "resources")
      if (length(unknown) > 0) {
        stop("Detected non-supported field name in argument 'args'. The BatchJobs backend only supports 'resources': ", paste(sQuote(unknown), collapse=", "))
      }
      for (name in names) config[[name]] <- args[[name]]
      .Deprecated(msg="Argument 'args' is deprecated in future.BatchJobs (>= 0.13.0). Please use argument 'resources' instead.")
    }
  }
  
  future$config <- config
 
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
  NextMethod("print")

  ## BatchJobs specific
  
  ## Type of BatchJobs future
  config <- x$config
  printf("BatchJobs cluster functions: %s\n", sQuote(config$cluster.functions$name))
  
  ## Ask for status once
  status <- status(x)
  printf("BatchJobs status: %s\n", paste(sQuote(status), collapse=", "))
  if ("error" %in% status) printf("Error: %s\n", loggedError(x))

  reg <- config$reg
  if (isNA(status)) {
    printf("BatchJobs %s: Not found (happens when finished and deleted)\n", class(reg))
  } else {
    printf("BatchJobs Registry:\n  ")
    print(reg)
  }
}


status <- function(...) UseMethod("status")
finished <- function(...) UseMethod("finished")
loggedError <- function(...) UseMethod("loggedError")
loggedOutput <- function(...) UseMethod("loggedOutput")

#' Status of BatchJobs future
#'
#' @param future The future.
#' @param \ldots Not used.
#'
#' @return A character vector or a logical scalar.
#'
#' @aliases status finished value
#'          loggedError loggedOutput
#' @keywords internal
#'
#' @export
#' @export status
#' @export finished
#' @export value
#' @export loggedError
#' @export loggedOutput
#' @importFrom BatchJobs getStatus
#' @importFrom utils file_test
status.BatchJobsFuture <- function(future, ...) {
  ## WORKAROUND: Avoid warnings on partially matched arguments
  getStatus <- function(...) {
    ## Temporarily disable BatchJobs output?
    ## (i.e. messages and progress bars)
    debug <- getOption("future.debug", FALSE)
    batchjobsOutput <- getOption("future.BatchJobs.output", debug)
    if (!batchjobsOutput) {
      oopts <- options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")
    } else {
      oopts <- list()
    }
    on.exit(options(oopts))
    oopts <- c(oopts, options(warnPartialMatchArgs=FALSE))
    BatchJobs::getStatus(...)
  } ## getStatus()

  config <- future$config
  reg <- config$reg
  if (!inherits(reg, "Registry")) return(NA_character_)
  ## Closed and deleted?
  if (!file_test("-d", reg$file.dir)) return(NA_character_)

  jobid <- config$jobid
  if (is.na(jobid)) return("not submitted")
  status <- getStatus(reg, ids=jobid)
  status <- (unlist(status) == 1L)
  status <- status[status]
  status <- sort(names(status))
  status <- setdiff(status, c("n"))
  status
} # status()



#' @export
#' @keywords internal
finished.BatchJobsFuture <- function(future, ...) {
  status <- status(future)
  if (isNA(status)) return(NA)
  any(c("done", "error", "expired") %in% status)
}

#' @export
#' @keywords internal
loggedError.BatchJobsFuture <- function(future, ...) {
  stat <- status(future)
  if (isNA(stat)) return(NULL)

  if (!finished(future)) {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    msg <- sprintf("%s ('%s') has not finished yet", class(future)[1L], label)
    stop(BatchJobsFutureError(msg, future=future))
  }

  if (!"error" %in% stat) return(NULL)

  config <- future$config
  reg <- config$reg
  jobid <- config$jobid
  msg <- getErrorMessages(reg, ids=jobid)
  msg <- paste(sQuote(msg), collapse=", ")
  msg
} # loggedError()


#' @importFrom BatchJobs getLogFiles
#' @export
#' @keywords internal
loggedOutput.BatchJobsFuture <- function(future, ...) {
  stat <- status(future)
  if (isNA(stat)) return(NULL)

  if (!finished(future)) {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    msg <- sprintf("%s ('%s') has not finished yet", class(future)[1L], label)
    stop(BatchJobsFutureError(msg, future=future))
  }

  if (!"error" %in% stat) return(NULL)

  config <- future$config
  reg <- config$reg
  jobid <- config$jobid
  pathname <- getLogFiles(reg, ids=jobid)
  bfr <- readLines(pathname)
  bfr
} # loggedOutput()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Future API
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom future resolved
#' @export
#' @keywords internal
resolved.BatchJobsFuture <- function(x, ...) {
  ## Has internal future state already been switched to be resolved
  resolved <- NextMethod("resolved")
  if (resolved) return(TRUE)

  ## If not, checks the BatchJobs registry status
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

  if (future$state == 'created') {
    future <- run(future)
  }

  stat <- status(future)
  if (isNA(stat)) {
    onMissing <- match.arg(onMissing)
    if (onMissing == "default") return(default)
    label <- future$label
    if (is.null(label)) label <- "<none>"
    stop(sprintf("The value no longer exists (or never existed) for Future ('%s') of class %s", label, paste(sQuote(class(future)), collapse=", ")))
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

#' @importFrom future getExpression
#' @importFrom BatchJobs addRegistryPackages batchExport batchMap
run.BatchJobsFuture <- function(future, ...) {
  if (future$state != 'created') {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    stop(sprintf("A future ('%s') can only be launched once.", label))
  }
  
  mdebug <- importFuture("mdebug")
  assignConf <- importBatchJobs("assignConf")

  ## WORKAROUND: Avoid warnings on partially matched arguments
  ## and "dollar" field name.
  submitJobs <- function(...) {
    oopts <- options(warnPartialMatchArgs=FALSE, warnPartialMatchDollar=FALSE)
    keep <- !unlist(lapply(oopts, FUN=function(x) is.null(x) || !x))
    oopts <- oopts[keep]
    if (length(oopts) > 0L) on.exit(options(oopts))

    BatchJobs::submitJobs(...)
  }


  getClusterFunctions <- function() {
    getBatchJobsConf <- importBatchJobs("getBatchJobsConf")
    getClusterFunctions <- importBatchJobs("getClusterFunctions")
    getClusterFunctions(getBatchJobsConf())
  }

  ## Assert that the process that created the future is
  ## also the one that evaluates/resolves/queries it.
  assertOwner <- importFuture("assertOwner")
  assertOwner(future)

  ## Temporarily disable BatchJobs output?
  ## (i.e. messages and progress bars)
  debug <- getOption("future.debug", FALSE)
  batchjobsOutput <- getOption("future.BatchJobs.output", debug)
  if (!batchjobsOutput) {
    oopts <- options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")
  } else {
    oopts <- list()
  }
  on.exit(options(oopts))

  expr <- getExpression(future)

  ## Always evaluate in local environment
  expr <- substitute(local(expr), list(expr=expr))

  reg <- future$config$reg
  stopifnot(inherits(reg, "Registry"))

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
      mdebug(msg)
    }

    ## COMMENTS:
    ## * The below can be removed with fail (>= 1.3) AND
    ##   BatchJobs (>= 1.7) /HB 2015-10-20
    ## * fail 1.3 is on CRAN but BatchJobs needs to be
    ##   updated too /HB 2016-05-01
    ## BatchJobs::loadExports() ignores exported variables that
    ## start with a period.
    ## Details: https://github.com/tudo-r/BatchJobs/issues/103
    bad <- grepl("^[.]", names(globals))
    if (any(bad)) {
      names <- names(globals)[bad]
      globalsToEncode <- c(globalsToEncode, names)
      msg <- sprintf("WORKAROUND: BatchJobs does not support exported variables that start with a period (see https://github.com/tudo-r/BatchJobs/issues/103). Encoding/decoding the following global variables: %s", hpaste(sQuote(names)))
      mdebug(msg)
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

      ## (d) Record variables which to be renamed by the future
      globals <- c(globals, list(R_ASYNC_GLOBALS_TO_RENAME=globalsToDecode))

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
  jobid <- batchMap(reg, fun=geval, list(expr), more.args=list(substitute=TRUE))

  ## 2. Update
  future$config$jobid <- jobid
  mdebug("Created %s future #%d", class(future)[1], jobid)


  ## FIXME: This one too here?!?  /HB 2016-03-20 Issue #49
  ## If/when makeRegistry() attaches BatchJobs, we need
  ## to prevent it from overriding configuration already set.
  ## Note, this will be reset by the first on.exit() above.
  oopts2 <- options(BatchJobs.load.config = FALSE)
  oopts <- c(oopts, oopts2)
  oopts2 <- NULL

  ## 3. Create BatchJobs configuration backend?
  conf <- future$conf
  if (is.null(conf)) {
    ## 3. Create BatchJobs backend configuration
    cluster.functions <- future$config$cluster.functions
    stopifnot(!is.null(cluster.functions))
    conf <- makeBatchJobsConf(cluster.functions)
  }

  ## 4. Use BatchJobs configuration backend
  if (debug) {
    mprintf("Setting BatchJobs configuration:\n")
    mstr(as.list(conf))
  }
  assignConf(conf)

  ## WORKAROUND: (For multicore and OS X only)
  if (conf$cluster.functions$name == "Multicore") {
    ## On some OS X systems, a system call to 'ps' may output an error message
    ## "dyld: DYLD_ environment variables being ignored because main executable
    ##  (/bin/ps) is setuid or setgid" to standard error that is picked up by
    ## BatchJobs which incorrectly tries to parse it.  By unsetting all DYLD_*
    ## environment variables, we avoid this message.  For more info, see:
    ## * https://github.com/tudo-r/BatchJobs/issues/117
    ## * https://github.com/HenrikBengtsson/future.BatchJobs/issues/59
    ## /HB 2016-05-07
    dyld_envs <- tryCatch({
      envs <- list()
      res <- system2("ps", stdout=TRUE, stderr=TRUE)
      if (any(grepl("DYLD_", res))) {
        envs <- Sys.getenv()
        envs <- envs[grepl("^DYLD_", names(envs))]
        if (length(envs) > 0L) lapply(names(envs), FUN=Sys.unsetenv)
      }
      envs
    }, error = function(ex) list())
  }

  ## 5. Submit
  future$state <- 'running'
  resources <- future$config$resources
  if (is.null(resources)) resources <- list()
  job.delay <- future$config$job.delay

  submitJobs(reg, ids=jobid, resources=resources, job.delay=job.delay)
  
  mdebug("Launched future #%d", jobid)

  invisible(future)
} ## run()


await <- function(...) UseMethod("await")

#' Awaits the value of a BatchJobs future
#'
#' @param future The future.
#' @param cleanup If TRUE, the registry is completely removed upon
#' success, otherwise not.
#' @param timeout Total time (in seconds) waiting before generating an error.
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
#' after being called the actual asynchronous future may be removed
#' and will no longer available in subsequent calls.  If called
#' again, an error may be thrown.
#'
#' @export
#' @importFrom BatchJobs getErrorMessages loadResult removeRegistry
#' @keywords internal
await.BatchJobsFuture <- function(future, cleanup = TRUE, timeout = getOption("future.wait.timeout", 30*24*60*60), delta=getOption("future.wait.interval", 0.2), alpha=getOption("future.wait.alpha", 1.01), ...) {
  mdebug <- importFuture("mdebug")
  stopifnot(is.finite(timeout), timeout >= 0)
  stopifnot(is.finite(alpha), alpha > 0)

  debug <- getOption("future.debug", FALSE)
  mdebug("Polling...")

  expr <- future$expr
  config <- future$config
  reg <- config$reg
  jobid <- config$jobid

  ## It appears that occassionally a job can shown as 'expired'
  ## just before becoming 'done'.  It's odd and should be reported
  ## but here's a workaround that won't trust 'expired' without
  ## confirming it several times with delay.
  final_countdown <- 5L
  final_state <- NULL
  final_state_prev <- NULL
  finish_states <- c("done", "error", "expired")

  t0 <- Sys.time()
  dt <- 0
  iter <- 1L
  interval <- delta
  stat <- NULL
  expired_countdown <- 0L
  finished <- FALSE
  while (dt <= timeout || expired_countdown > 0) {
    stat <- status(future)
    if (isNA(stat)) {
      finished <- TRUE
      break
    }
    mdebug(sprintf("Poll #%d (%s): status = %s", iter, format(round(dt, digits = 2L)), paste(stat, collapse = ", ")))
    
    if (any(finish_states %in% stat)) {
      final_state <- intersect(stat, finish_states)

      ## ROBUSTNESS: Don't trust a single 'expired' status.
      ## Need to see that several times before believing it.
      ## See BatchJobs Issue #70.
      if ("expired" %in% final_state) {
        if (!identical(final_state, final_state_prev)) {
          final_state_prev <- final_state
    	  expired_countdown <- 20L
          interval <- delta
        } else {
          expired_countdown <- expired_countdown - 1L
          mdebug(" 'expired' status countdown: %d", expired_countdown)
          if (expired_countdown == 0L) {
            finished <- TRUE
            break
  	  }
        }
      } else {
        finished <- TRUE
        break
      }
    }

    ## Wait
    Sys.sleep(interval)
    interval <- alpha*interval

    iter <- iter + 1L
    dt <- difftime(Sys.time(), t0)
  }

  res <- NULL
  if (finished) {
    mdebug("Results:")
    label <- future$label
    if (is.null(label)) label <- "<none>"
    if ("done" %in% stat) {
      res <- loadResult(reg, id=jobid)
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobError in %s ('%s'): %s", class(future)[1], label, loggedError(future))
      stop(BatchJobsFutureError(msg, future=future, output=loggedOutput(future)))
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobExpiration: Future ('%s') expired: %s", label, reg$file.dir)
      stop(BatchJobsFutureError(msg, future=future, output=loggedOutput(future)))
    } else if (isNA(stat)) {
      msg <- sprintf("BatchJobDeleted: Cannot retrieve value. Future ('%s') deleted: %s", label, reg$file.dir)
      stop(BatchJobsFutureError(msg, future=future))
    }
    if (debug) { mstr(res) }
  } else {
    cleanup <- FALSE
    msg <- sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchronous evaluation for future ('%s') is still running: %s", iter-1L, interval, label, reg$file.dir)
    stop(BatchJobsFutureError(msg, future=future))
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
#' @param times The number of tries before giving up.
#' @param delta The delay interval (in seconds) between retries.
#' @param alpha A multiplicative penalty increasing the delay
#' for each failed try.
#' @param \ldots Not used.
#'
#' @return (invisibly) TRUE if deleted and FALSE otherwise.
#'
#' @export
#' @importFrom BatchJobs removeRegistry
#' @importFrom utils file_test
#' @keywords internal
delete.BatchJobsFuture <- function(future, onRunning=c("warning", "error", "skip"), onFailure=c("error", "warning", "ignore"), onMissing=c("ignore", "warning", "error"), times=10L, delta=getOption("future.wait.interval", 1.0), alpha=getOption("future.wait.alpha", 1.01), ...) {
  mdebug <- importFuture("mdebug")
  
  onRunning <- match.arg(onRunning)
  onMissing <- match.arg(onMissing)
  onFailure <- match.arg(onFailure)

  debug <- getOption("future.debug", FALSE)

  ## Identify registry
  config <- future$config
  reg <- config$reg
  path <- reg$file.dir

  ## Already deleted?
  if (is.null(path) || !file_test("-d", path)) {
    if (onMissing %in% c("warning", "error")) {
      msg <- sprintf("Cannot remove BatchJob registry, because directory does not exist: %s", sQuote(path))
      mdebug("delete(): %s", msg)
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        stop(BatchJobsFutureError(msg, future=future))
      }
    }
    return(invisible(TRUE))
  }


  ## Is the future still not resolved?  If so, then...
  if (!resolved(future)) {
    if (onRunning == "skip") return(invisible(TRUE))
    status <- status(future)
    label <- future$label
    if (is.null(label)) label <- "<none>"
    msg <- sprintf("Will not remove BatchJob registry, because is appears to hold a non-resolved future (%s; state=%s; BatchJobs status=%s): %s", sQuote(label), sQuote(future$state), paste(sQuote(status), collapse=", "), sQuote(path))
    mdebug("delete(): %s", msg)
    if (onRunning == "warning") {
      warning(msg)
      return(invisible(TRUE))
    } else if (onRunning == "error") {
      stop(BatchJobsFutureError(msg, future=future))
    }
  }


  ## To simplify post mortem troubleshooting in non-interactive sessions,
  ## should the BatchJobs registry files be removed or not?
  mdebug("delete(): Option 'future.delete=%s", sQuote(getOption("future.delete", "<NULL>")))
  if (!getOption("future.delete", interactive())) {
    status <- status(future)
    if (any(c("error", "expired") %in% status)) {
      msg <- sprintf("Will not remove BatchJob registry, because the status of the BatchJobs was %s and option 'future.delete' is FALSE or running in an interactive session: %s", paste(sQuote(status), collapse=", "), sQuote(path))
      mdebug("delete(): %s", msg)
      warning(msg)
      return(invisible(FALSE))
    }
  }


  ## Try to delete registry
  interval <- delta
  for (kk in seq_len(times)) {
    try(removeRegistry(reg, ask="no"), silent=TRUE)
    if (!file_test("-d", path)) break
    Sys.sleep(interval)
    interval <- alpha*interval
  }


  ## Success?
  if (file_test("-d", path)) {
    if (onFailure %in% c("warning", "error")) {
      msg <- sprintf("Failed to remove BatchJob registry: %s", sQuote(path))
      mdebug("delete(): %s", msg)
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        stop(BatchJobsFutureError(msg, future=future))
      }
    }
    return(invisible(FALSE))
  }

  mdebug("delete(): BatchJobs registry deleted: %s", sQuote(path))

  invisible(TRUE)
} # delete()


add_finalizer <- function(...) UseMethod("add_finalizer")

add_finalizer.BatchJobsFuture <- function(future, ...) {
  ## Register finalizer (will clean up registries etc.)

  reg.finalizer(future, f=function(gcenv) {
    if (inherits(future, "BatchJobsFuture") && "future.BatchJobs" %in% loadedNamespaces()) {
      try({
        delete(future, onRunning="skip", onMissing="ignore", onFailure="warning")
      })
    }
  }, onexit=TRUE)

  invisible(future)
}
