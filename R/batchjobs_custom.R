#' BatchJobs custom futures
#'
#' A custom BatchJobs future uses a BatchJobs configuration file
#' that defines the BatchJobs configuration environment, e.g. \file{.BatchJobs.R}.
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param pathname The pathname to a BatchJobs configuration file.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @export
batchjobs_custom <- function(expr, envir=parent.frame(), substitute=TRUE, pathname, ...) {
  sourceConfFiles <- importBatchJobs("sourceConfFiles")

  if (substitute) expr <- substitute(expr)
  stopifnot(length(pathname) == 1L, is.character(pathname), file_test("-f", pathname))

  conf <- sourceConfFiles(pathname)

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            conf=conf, ...)

  future <- run(future)

  future
}
class(batchjobs_custom) <- c("batchjobs_custom", "batchjobs", "multiprocess", "future", "function")
