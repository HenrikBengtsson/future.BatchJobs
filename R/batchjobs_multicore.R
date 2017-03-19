#' BatchJobs multicore futures
#'
#' A BatchJobs multicore future is an asynchronous multiprocess
#' future that will be evaluated in a background R session.\cr
#' \cr
#' \emph{We highly recommend using \code{\link[future]{multisession}}
#' (sic!) futures of the \pkg{future} package instead of
#' multicore BatchJobs futures.}
#'
#' @inheritParams BatchJobsFuture
#' @param workers The number of multicore processes to be
#' available for concurrent BatchJobs multicore futures.
#' @param \ldots Additional arguments passed
#' to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @details
#' BatchJobs multicore futures rely on the BatchJobs backend set
#' up by \code{\link[BatchJobs]{makeClusterFunctionsMulticore}()}.
#' The BatchJobs multicore backend only works on operating systems
#' supporting the \code{ps} command-line tool, e.g. Linux and OS X.
#' However, they are not supported on neither Windows nor Solaris
#' Unix (because \code{ps -o ucomm=} is not supported).  When not
#' supported, it falls back to \code{\link{batchjobs_local}}.
#'
#' \emph{Warning: For multicore BatchJobs, the \pkg{BatchJobs}
#' package uses a built-in algorithm for load balancing based on
#' other processes running on the same machine.  This is done
#' in order to prevent the machine's CPU load to blow up.
#' Unfortunately, the BatchJobs criteria for handling this often
#' results in starvation, that is, long waiting times before
#' launching jobs.  The risk for this is particularly high if
#' there are other R processes running on the same machine
#' including those by other users.
#' See also \url{https://github.com/tudo-r/BatchJobs/issues/99}.
#' \bold{Conclusion:} We highly recommend using
#' \code{\link[future]{multisession}} futures of the
#' \pkg{future} package instead of multicore BatchJobs futures.}
#'
#' Also, despite the name, BatchJobs multicore futures are in
#' function closer related to \link[future:multisession]{multisession}
#' futures than \link[future:multicore]{multicore} futures,
#' both provided by the \pkg{future} package.  This is because
#' BatchJobs spawns off background R sessions rather than forking
#' the current R process as the name otherwise might imply (at least
#' that is how the term "multicore processing" is typically used
#' in the R world).
#'
#' @importFrom BatchJobs makeClusterFunctionsMulticore
#' @importFrom future availableCores
#' @export
#' @keywords internal
batchjobs_multicore <- function(expr, envir=parent.frame(), substitute=TRUE, globals=TRUE, label="BatchJobs", workers=availableCores(constraints="multicore"), job.delay=FALSE, ...) {
  if (substitute) expr <- substitute(expr)

  if (is.null(workers)) workers <- availableCores(constraints="multicore")
  stopifnot(length(workers) == 1L, is.numeric(workers),
            is.finite(workers), workers >= 1L)

  ## Fall back to batchjobs_local if multicore processing is not supported
  if (workers == 1L || isOS("windows") || isOS("solaris") || availableCores(constraints="multicore") == 1L) {
    ## covr: skip=1
    return(batchjobs_local(expr, envir=envir, substitute=FALSE, globals=globals, label=label, job.delay=job.delay, ...))
  }

  oopts <- options(mc.cores=workers)
  on.exit(options(oopts))

  ncpus0 <- availableCores(constraints="multicore")
  ncpus <- workers
  stopifnot(ncpus >= 2L)

  ## PROBLEM:
  ## BatchJobs' multicore cluster functions tries to be responsive to the overall
  ## CPU load of the machine (as reported by Linux command 'uptime') and it will
  ## not submit new jobs if the load is greater than its 'max.load' parameter.
  ## This parameter is by default set to one less than number of available cores
  ## on the machine (as reported by parallel::detectCores()).  This way it tries
  ## to leave some leeway for other processes avoiding clogging up the machine.
  ## If 'mc.cores' is set, that it taken as the number of available cores instead.
  ## See BatchJobs:::makeWorker() for code.  However, the CPU load is still relative
  ## to the true number of cores available.  In other words, the check that the
  ## observed CPU load is less than 'max.load' (== mc.cores-1) is not correct and
  ## may end up never to be meet, resulting in an endless waiting to submit jobs.
  ##
  ## WORKAROUND:
  ## A better estimate may be to set 'max.load' to be parallel::detectCores()-1.
  ## However, it appears that that may also stall the processing in some cases.
  ## Because of this, we set the limit to +Inf.  This should be alright because
  ## max.jobs=ncpus (also the default if not specified).  If a user wish to use
  ## other settings, this can be done via a custom .BatchJobs.R file.
  ## /HB 2016-05-16
  max.load <- +Inf
  cf <- makeClusterFunctionsMulticore(ncpus=ncpus, max.jobs=ncpus, max.load=max.load)

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            globals=globals,
			    label=label,
                            cluster.functions=cf,
			    job.delay=job.delay, ...)

  if (!future$lazy) future <- run(future)

  future
}
class(batchjobs_multicore) <- c("batchjobs_multicore", "batchjobs", "multiprocess", "future", "function")
