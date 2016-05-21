#' BatchJobs local and interactive futures
#'
#' A \emph{local} and an \emph{interactive} BatchJobs future is synchronous
#' and uni-process, will be evaluated immediately, and will block until the
#' future is resolved.
#' The difference between a local and an interactive BatchJobs is that
#' the former will be evaluated in a separate background R session whereas
#' the latter will be evaluated in the current R session.  Furthermore,
#' the interactive one will not be evaluated in a local environment and
#' therefore any assignments will be made to calling environment
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @seealso
#' An alternative to BatchJobs local futures is to use
#' \link[future:cluster]{cluster} futures of the \pkg{future}
#' with a single local background session, i.e.
#' \code{plan(cluster, workers="localhost")}.
#'
#' An alternative to BatchJobs interactive futures is to use
#' \link[future:transparent]{transparent} futures of the \pkg{future}.
#'
#' Internally \code{\link[BatchJobs]{makeClusterFunctionsLocal}()} and
#' \code{\link[BatchJobs]{makeClusterFunctionsInteractive}()} are used
#' to create the BatchJobs cluster-function backends.
#'
#' @importFrom BatchJobs makeClusterFunctionsLocal
#' @aliases batchjobs_interactive
#' @export
batchjobs_local <- function(expr, envir=parent.frame(), substitute=TRUE, ...) {
  if (substitute) expr <- substitute(expr)

  cf <- makeClusterFunctionsLocal()

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            cluster.functions=cf, ...)

  future <- run(future)

  future
}
class(batchjobs_local) <- c("batchjobs_local", "batchjobs", "uniprocess", "future", "function")
