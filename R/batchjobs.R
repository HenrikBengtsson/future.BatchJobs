#' A BatchJobs future represents a future that is resolved by one of the many BatchJobs backends
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param backend The BatchJobs backend to use, cf. \code{\link{backend}()}.
#' @param resources A named list of resources needed by this future.
#' @param \ldots Additional arguments pass to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @export
batchjobs <- function(expr, envir=parent.frame(), substitute=TRUE, backend=NULL, resources=list(), ...) {
  if (substitute) expr <- substitute(expr)

  ## 1. Create
  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            backend=backend, resources=resources, ...)

  ## 2. Launch
  future <- run(future)

  future
}
class(batchjobs) <- c("batchjobs", "future", "function")
