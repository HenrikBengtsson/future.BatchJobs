#' BatchJobs local and interactive futures
#'
#' A BatchJobs local future is an synchronous uniprocess future that
#' will be evaluated in a background R session.
#' A BatchJobs interactive future is an synchronous uniprocess future
#' that will be evaluated in the current R session (and variables will
#' be assigned to the calling environment rather than to a local one).
#' Both types of futures will block until the futures are resolved.
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param globals (optional) a logical, a character vector, a named list, or a \link[globals]{Globals} object.  If TRUE, globals are identified by code inspection based on \code{expr} and \code{tweak} searching from environment \code{envir}.  If FALSE, no globals are used.  If a character vector, then globals are identified by lookup based their names \code{globals} searching from environment \code{envir}.  If a named list or a Globals object, the globals are used as is.
#' @param job.delay (optional) Passed as is to \code{\link[BatchJobs]{submitJobs}()}.
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
batchjobs_local <- function(expr, envir=parent.frame(), substitute=TRUE, globals=TRUE, job.delay=FALSE, ...) {
  if (substitute) expr <- substitute(expr)

  cf <- makeClusterFunctionsLocal()

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            globals=globals,
			    cluster.functions=cf,
			    job.delay=job.delay, ...)

  future <- run(future)

  future
}
class(batchjobs_local) <- c("batchjobs_local", "batchjobs", "uniprocess", "future", "function")
