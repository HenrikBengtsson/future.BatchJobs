#' BatchJobs conf futures
#'
#' A conf BatchJobs future sources one or more BatchJobs configuration
#' files (R source scripts) to define the BatchJobs configuration
#' environment, e.g. \file{.BatchJobs.R}.
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param pathnames Pathnames to one or more BatchJobs configuration
#' files to be loaded in order.  If NULL, then the \pkg{BatchJobs}
#' package will search for such configuration files.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @details
#' If \code{pathnames=NULL} (default), the \pkg{BatchJobs} is configured
#' to use (in order) all of following configuration files (if they exist):
#' \itemize{
#'  \item \code{system("etc", "BatchJobs_global_config.R", package="BatchJobs")}
#'  \item \code{~/.BatchJobs.R} (in user's home directory)
#'  \item \code{.BatchJobs.R} (in the current directory)
#' }
#'
#' @export
batchjobs_conf <- function(expr, envir=parent.frame(), substitute=TRUE, pathnames=NULL, ...) {
  findConfigs <- importBatchJobs("findConfigs")
  sourceConfFiles <- importBatchJobs("sourceConfFiles")

  if (substitute) expr <- substitute(expr)

  if (is.null(pathnames)) {
    ## This is now BatchJobs searches, cf. BatchJobs:::readConfs()
    path <- find.package("BatchJobs")
    pathnames  <- findConfigs(path)
  }

  stopifnot(length(pathnames) >= 1L, is.character(pathnames))
  for (pathname in pathnames) {
    if (!file_test("-f", pathname)) stop("File not found: ", sQuote(pathname))
  }
  conf <- sourceConfFiles(pathnames)

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            conf=conf, ...)
  future$pathnames <- pathnames
  future <- run(future)

  future
}
class(batchjobs_conf) <- c("batchjobs_conf", "batchjobs", "multiprocess", "future", "function")
