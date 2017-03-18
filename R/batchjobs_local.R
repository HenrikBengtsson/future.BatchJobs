#' BatchJobs local and interactive futures
#'
#' A BatchJobs local future is an synchronous uniprocess future that
#' will be evaluated in a background R session.
#' A BatchJobs interactive future is an synchronous uniprocess future
#' that will be evaluated in the current R session (and variables will
#' be assigned to the calling environment rather than to a local one).
#' Both types of futures will block until the futures are resolved.
#'
#' @inheritParams BatchJobsFuture
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @details
#' BatchJobs local futures rely on the BatchJobs backend set
#' up by \code{\link[BatchJobs]{makeClusterFunctionsLocal}()}
#' and BatchJobs interactive futures on the one set up by
#' \code{\link[BatchJobs]{makeClusterFunctionsInteractive}()}.
#' These are supported by all operating systems.
#'
#' An alternative to BatchJobs local futures is to use
#' \link[future:cluster]{cluster} futures of the \pkg{future}
#' package with a single local background session, i.e.
#' \code{plan(cluster, workers="localhost")}.
#'
#' An alternative to BatchJobs interactive futures is to use
#' \link[future:transparent]{transparent} futures of the
#' \pkg{future} package.
#'
#' @example incl/batchjobs_local.R
#'
#' @importFrom BatchJobs makeClusterFunctionsLocal
#' @aliases batchjobs_interactive
#' @export
batchjobs_local <- function(expr, envir=parent.frame(), substitute=TRUE, globals=TRUE, label="BatchJobs", workers=1L, job.delay=FALSE, ...) {
  if (substitute) expr <- substitute(expr)

  cf <- makeClusterFunctionsLocal()

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            globals=globals,
			    label=label,
			    cluster.functions=cf,
                            workers=workers,
			    job.delay=job.delay, ...)

  if (!future$lazy) future <- run(future)

  future
}
class(batchjobs_local) <- c("batchjobs_local", "batchjobs", "uniprocess", "future", "function")
