#' BatchJobs conf futures
#'
#' A conf BatchJobs future uses one or more BatchJobs configuration
#' files to define the BatchJobs configuration environment,
#' e.g. \file{.BatchJobs.R}.
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param pathnames Pathnames to one or more BatchJobs configuration files
#' to be loaded in order.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @export
batchjobs_conf <- function(expr, envir=parent.frame(), substitute=TRUE, pathnames, ...) {
  sourceConfFiles <- importBatchJobs("sourceConfFiles")

  if (substitute) expr <- substitute(expr)
  stopifnot(length(pathnames) >= 1L, is.character(pathnames))
  for (pathname in pathnames) {
    if (!file_test("-f", pathname)) stop("File not found: ", sQuote(pathname))
  }
  conf <- sourceConfFiles(pathnames)

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            conf=conf, ...)

  future <- run(future)

  future
}
class(batchjobs_conf) <- c("batchjobs_conf", "batchjobs", "multiprocess", "future", "function")
