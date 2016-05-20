#' BatchJobs multicore futures
#'
#' A multicore BatchJobs future is asynchronous and multiprocess,
#' will be evaluated immediately and will not block.
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param workers The number of multicore processes to be available
#' for concurrent BatchJobs multicore futures.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @seealso
#' An alternative to BatchJobs multicore futures is to use
#' \link[future:multicore]{multicore} futures of the \pkg{future}.
#'
#' Internally \code{\link[BatchJobs]{makeClusterFunctionsMulticore}()}
#' is used to create the BatchJobs cluster-function backend.
#'
#' @importFrom BatchJobs makeClusterFunctionsMulticore
#' @importFrom future availableCores
#' @export
batchjobs_multicore <- function(expr, envir=parent.frame(), substitute=TRUE, workers=availableCores(constraints="multicore"), ...) {
  if (substitute) expr <- substitute(expr)

  stopifnot(length(workers) == 1L, is.numeric(workers),
            is.finite(workers), workers >= 1L)

  if (workers == 1L || availableCores(constraints="multicore") == 1L) {
    ## covr: skip=1
    return(batchjobs_local(expr, envir=envir, substitute=FALSE, ...))
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
                            cluster.functions=cf,
                            backend="multicore", ...)

  future <- run(future)

  future
}
class(batchjobs_multicore) <- c("batchjobs_multicore", "batchjobs", "multiprocess", "future", "function")