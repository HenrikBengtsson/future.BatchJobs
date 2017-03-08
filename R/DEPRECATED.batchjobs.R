#' A BatchJobs future represents a future that is resolved by one of the many BatchJobs backends
#'
#' \emph{This function is defunct.  Please use \code{plan()} with one
#' of the available \code{batchjobs_*} future strategies.}
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param backend The BatchJobs backend to use.
#' @param workers (optional) Additional specification for the backend
#' workers.  If NULL, the default is used.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @export
#' @keywords internal
batchjobs <- function(expr, envir=parent.frame(), substitute=TRUE, backend=NULL, workers=NULL, ...) {

  .Defunct(msg=sprintf("plan(batchjobs, backend='%s') is defunct.  Please use a corresponding plan(batchjobs_*) setup", backend))
}
class(batchjobs) <- c("batchjobs", "future", "function")
