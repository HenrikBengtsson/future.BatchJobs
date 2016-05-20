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
#' @export
batchjobs_multicore <- function(expr, envir=parent.frame(), substitute=TRUE, workers=availableCores(constraints="multicore"), ...) {
  if (substitute) expr <- substitute(expr)

  if (workers == 1L || availableCores(constraints="multicore") == 1L) {
    ## covr: skip=1
    return(batchjobs_local(expr, envir=envir, substitute=FALSE, ...))
  }

  oopts <- options(mc.cores=workers)
  on.exit(options(oopts))

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            backend="multicore", ...)

  future <- run(future)

  future
}
class(batchjobs_multicore) <- c("batchjobs_multicore", "batchjobs", "multiprocess", "future", "function")
