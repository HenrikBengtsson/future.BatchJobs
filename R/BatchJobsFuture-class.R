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
#' @importFrom future Future getGlobalsAndPackages
#' @importFrom BatchJobs submitJobs
#' @keywords internal
BatchJobsFuture <- function(expr=NULL, envir=parent.frame(), substitute=TRUE, globals=TRUE, packages=NULL, label="BatchJobs", conf=NULL, cluster.functions=NULL, resources=list(), workers=NULL, job.delay=FALSE, finalize=getOption("future.finalize", TRUE), ...) {
  if (substitute) expr <- substitute(expr)

  ## Record globals
  gp <- getGlobalsAndPackages(expr, envir = envir, globals = globals)

  future <- Future(expr = gp$expr, envir = envir, substitute = FALSE,
                   globals = gp$globals,
                   packages = unique(c(packages, gp$packages)),
                   label = label,
                   ...)

  future <- as_BatchJobFuture(future,
                             conf=conf,
                             cluster.functions=cluster.functions,
                             resources=resources,
                             workers=workers,
                             job.delay=job.delay,
                             finalize=finalize)

  future
}


## Helper function to create a BatchtoolsFuture from a vanilla Future
#' @importFrom utils file_test
as_BatchJobFuture <- function(future, conf=NULL, cluster.functions=NULL, resources=list(), workers=NULL, job.delay=FALSE, finalize=getOption("future.finalize", TRUE), ...) {
  if (!is.null(conf)) {
    stop_if_not(is.environment(conf))
  }

  if (!is.null(cluster.functions)) {
    stop_if_not(is.list(cluster.functions))
    conf$cluster.functions <- cluster.functions
  }

  if (is.function(workers)) workers <- workers()
  if (!is.null(workers)) {
    stop_if_not(length(workers) >= 1)
    if (is.numeric(workers)) {
      stop_if_not(!anyNA(workers), all(workers >= 1))
    } else {
      stop("Argument 'workers' should be either a numeric or a function: ",
           mode(workers))
    }
  }

  stop_if_not(is.list(resources))

  stop_if_not(is.logical(job.delay) || is.function(job.delay))

  ## BatchJobs configuration
  future$config <- list(
    reg               = NULL,
    jobid             = NA_integer_,
    conf              = conf,
    cluster.functions = cluster.functions,
    resources         = resources,
    job.delay         = job.delay,
    finalize          = finalize
  )

  structure(future, class=c("BatchJobsFuture", class(future)))
}


#' Prints a BatchJobs future
#'
#' @param x An BatchJobsFuture object
#' @param \ldots Not used.
#'
#' @export
#' @keywords internal
print.BatchJobsFuture <- function(x, ...) {
  NextMethod()

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

  invisible(x)
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
    suppressDBIWarnings({
      BatchJobs::getStatus(...)
    })
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
  status[status == "done"] <- "finished"
  status
} # status()



#' @export
#' @keywords internal
finished.BatchJobsFuture <- function(future, ...) {
  status <- status(future)
  if (isNA(status)) return(NA)
  any(c("finished", "error", "expired") %in% status)
}

#' @export
#' @importFrom BatchJobs getErrorMessages
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
  resolved <- NextMethod()
  if (resolved) return(TRUE)

  ## If not, checks the BatchJobs registry status
  resolved <- finished(x)
  if (is.na(resolved)) return(FALSE)

  resolved
}

#' @importFrom future result
#' @export
#' @keywords internal
result.BatchJobsFuture <- function(future, cleanup = TRUE, ...) {
  ## Has the value already been collected?
  result <- future$result
  if (inherits(result, "FutureResult")) return(result)

  ## Has the value already been collected? - take two
  if (future$state %in% c("finished", "failed", "interrupted")) {
    return(NextMethod())
  }

  if (future$state == "created") {
    future <- run(future)
  }

  stat <- status(future)
  if (isNA(stat)) {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    stop(sprintf("The result no longer exists (or never existed) for Future ('%s') of class %s", label, paste(sQuote(class(future)), collapse = ", "))) #nolint
  }

  result <- await(future, cleanup = FALSE)
  stop_if_not(inherits(result, "FutureResult"))
  future$result <- result
  future$state <- "finished"
  if (cleanup) delete(future)

  NextMethod()
}


#' @importFrom future run getExpression
#' @importFrom BatchJobs addRegistryPackages batchExport batchMap
#' @export
run.BatchJobsFuture <- function(future, ...) {
  if (future$state != 'created') {
    label <- future$label
    if (is.null(label)) label <- "<none>"
    stop(sprintf("A future ('%s') can only be launched once.", label))
  }
  
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

  ## (i) Create batchtools registry
  reg <- future$config$reg
  stop_if_not(is.null(reg) || inherits(reg, "Registry"))
  if (is.null(reg)) {
    if (debug) mprint("- Creating BatchJobs registry")
    config <- future$config
    stop_if_not(is.list(config))

    ## Create BatchJobs registry
    label <- future$label
    if (!is.null(label)) label <- as.character(label)
    reg <- tempRegistry(label = label)
    if (debug) mprint(reg)
    future$config$reg <- reg

    ## Register finalizer?
    if (config$finalize) future <- add_finalizer(future)
    config <- NULL
  }
  stop_if_not(inherits(reg, "Registry"))

  ## (ii) Attach packages that needs to be attached
  packages <- future$packages
  if (length(packages) > 0) {
    mdebugf("Attaching %d packages (%s) ...",
                    length(packages), hpaste(sQuote(packages)))

    ## Record which packages in 'pkgs' that are loaded and
    ## which of them are attached (at this point in time).
    isLoaded <- is.element(packages, loadedNamespaces())
    isAttached <- is.element(packages, attachedPackages())

    ## FIXME: Update the expression such that the new session
    ## will have the same state of (loaded, attached) packages.

    addRegistryPackages(reg, packages=packages)

    mdebugf("Attaching %d packages (%s) ... DONE",
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
  suppressDBIWarnings({
    jobid <- batchMap(reg, fun=geval, list(expr),
                      more.args=list(substitute=TRUE))
  })

  ## 2. Update
  future$config$jobid <- jobid
  mdebugf("Created %s future #%d", class(future)[1], jobid)


  ## FIXME: This one too here?!?  /HB 2016-03-20 Issue #49
  ## If/when makeRegistry() attaches BatchJobs, we need
  ## to prevent it from overriding configuration already set.
  ## Note, this will be reset by the first on.exit() above.
  oopts2 <- options(BatchJobs.load.config = FALSE)
  oopts <- c(oopts, oopts2)
  oopts2 <- NULL

  ## 3. Create BatchJobs configuration backend?
  conf <- future$config$conf
  if (is.null(conf)) {
    ## 3. Create BatchJobs backend configuration
    cluster.functions <- future$config$cluster.functions
    stop_if_not(!is.null(cluster.functions))
    conf <- makeBatchJobsConf(cluster.functions)
  }

  ## 4. Use BatchJobs configuration backend
  if (debug) {
    mprint("Setting BatchJobs configuration:")
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
  
  mdebugf("Launched future #%d", jobid)

  invisible(future)
} ## run()


#' @importFrom BatchJobs loadResult
#' @importFrom utils tail
await <- function(future, cleanup = TRUE, timeout = getOption("future.wait.timeout", 30*24*60*60), delta=getOption("future.wait.interval", 0.2), alpha=getOption("future.wait.alpha", 1.01), ...) {
  stop_if_not(is.finite(timeout), timeout >= 0)
  stop_if_not(is.finite(alpha), alpha > 0)

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
  finish_states <- c("finished", "error", "expired")

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
    mdebugf("Poll #%d (%s): status = %s", iter, format(round(dt, digits = 2L)), paste(stat, collapse = ", "))
    
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
          mdebugf(" 'expired' status countdown: %d", expired_countdown)
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

  ## PROTOTYPE RESULTS BELOW:
  prototype_fields <- NULL
  
  res <- NULL
  if (finished) {
    mdebug("Results:")
    label <- future$label
    if (is.null(label)) label <- "<none>"
    if ("finished" %in% stat) {
      res <- loadResult(reg, id=jobid)
      if (inherits(res, "FutureResult")) {
        if (is.null(res$stdout) && isTRUE(future$stdout)) {
          prototype_fields <- c(prototype_fields, "stdout")
          res$stdout <- loggedOutput(future)
        }
        if (result_has_errors(res)) cleanup <- FALSE
      }
    } else if ("error" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobError in %s ('%s'): %s", class(future)[1], label, loggedError(future))
      stop(BatchJobsFutureError(msg, future=future))
    } else if ("expired" %in% stat) {
      cleanup <- FALSE
      msg <- sprintf("BatchJobsExpiration: Future ('%s') expired (registry path %s).", label, reg$file.dir)
      output <- loggedOutput(future)
      hint <- unlist(strsplit(output, split = "\n", fixed = TRUE))
      hint <- hint[nzchar(hint)]
      hint <- tail(hint, n = 6L)
      if (length(hint) > 0) {
        hint <- paste(hint, collapse = "\n")
        msg <- sprintf("%s. The last few lines of the logged output:\n%s",
                       msg, hint)
      } else {
        msg <- sprintf("%s. No logged output exist.", msg)
      }
      stop(BatchJobsFutureError(msg, future=future))
    } else if (isNA(stat)) {
      msg <- sprintf("BatchJobDeleted: Cannot retrieve value. Future ('%s') deleted: %s", label, reg$file.dir)
      stop(BatchJobsFutureError(msg, future=future))
    }
    if (debug) mstr(res)
  } else {
    cleanup <- FALSE
    msg <- sprintf("AsyncNotReadyError: Polled for results %d times every %g seconds, but asynchronous evaluation for future ('%s') is still running: %s", iter-1L, interval, label, reg$file.dir)
    stop(BatchJobsFutureError(msg, future=future))
  }

  if (length(prototype_fields) > 0) {
    res$PROTOTYPE_WARNING <- sprintf("WARNING: The fields %s should be considered internal and experimental for now, that is, until the Future API for these additional features has been settled. For more information, please see https://github.com/HenrikBengtsson/future/issues/172", hpaste(sQuote(prototype_fields), maxHead = Inf, collapse = ", ", lastCollapse  = " and "))
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
      mdebugf("delete(): %s", msg)
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
    mdebugf("delete(): %s", msg)
    if (onRunning == "warning") {
      warning(msg)
      return(invisible(TRUE))
    } else if (onRunning == "error") {
      stop(BatchJobsFutureError(msg, future=future))
    }
  }

  ## Make sure to collect the results before deleting
  ## the internal batchtools registry
  result <- result(future, cleanup = FALSE)
  stop_if_not(inherits(result, "FutureResult"))

  ## To simplify post mortem troubleshooting in non-interactive sessions,
  ## should the BatchJobs registry files be removed or not?
  mdebugf("delete(): Option 'future.delete=%s", sQuote(getOption("future.delete", "<NULL>")))
  if (!getOption("future.delete", interactive())) {
    status <- status(future)
    res <- future$result
    if (inherits(res, "FutureResult")) {
      if (result_has_errors(res)) status <- unique(c("error", status))
    }
    mdebugf("delete(): status = %s", paste(sQuote(status), collapse = ", "))
    if (any(c("error", "expired") %in% status)) {
      msg <- sprintf("Will not remove BatchJob registry, because the status of the BatchJobs was %s and option 'future.delete' is FALSE or running in an interactive session: %s", paste(sQuote(status), collapse=", "), sQuote(path))
      mdebugf("delete(): %s", msg)
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
      mdebugf("delete(): %s", msg)
      if (onMissing == "warning") {
        warning(msg)
      } else if (onMissing == "error") {
        stop(BatchJobsFutureError(msg, future=future))
      }
    }
    return(invisible(FALSE))
  }

  mdebugf("delete(): BatchJobs registry deleted: %s", sQuote(path))

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
