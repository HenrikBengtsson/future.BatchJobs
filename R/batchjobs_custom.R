#' BatchJobs conf futures
#'
#' A conf BatchJobs future sources one or more BatchJobs configuration
#' files (R source scripts) to define the BatchJobs configuration
#' environment, e.g. \file{.BatchJobs.R}.
#'
#' @inheritParams BatchJobsFuture
#' @param conf A BatchJobs configuration environment.
#' @param pathname (alternative) Pathname to one or more BatchJobs
#' configuration files to be loaded in order.  If NULL, then the
#' \pkg{BatchJobs} package will search for such configuration files.
#' @param workers (optional) Additional specification for the backend
#' workers.  If NULL, the default is used.
#' @param resources A named list passed to the BatchJobs template (available as variable \code{resources}).
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @details
#' If \code{conf} is NULL (default), then the BatchJobs configuration will
#' be created from a set of BatchJobs configuration files (R script files)
#' as given by argument \code{pathname}.  If none are specified (default),
#' then \pkg{BatchJobs} is designed to use (in order) all of following
#' configuration files (if they exist):
#' \itemize{
#'  \item \code{system("etc", "BatchJobs_global_config.R", package="BatchJobs")}
#'  \item \code{~/.BatchJobs.R} (in user's home directory)
#'  \item \code{.BatchJobs.R} (in the current directory)
#' }
#'
#' @export
batchjobs_custom <- function(expr, envir=parent.frame(), substitute=TRUE, globals=TRUE, label="BatchJobs", conf=NULL, pathname=NULL, workers=Inf, resources=list(), job.delay=FALSE, ...) {
  findConfigs <- importBatchJobs("findConfigs")
  sourceConfFiles <- importBatchJobs("sourceConfFiles")

  if (substitute) expr <- substitute(expr)

  if (is.null(conf)) {
    if (is.null(pathname)) {
      ## This is how BatchJobs searches, cf. BatchJobs:::readConfs()
      path <- find.package("BatchJobs")
      pathname  <- findConfigs(path)
    }

    stopifnot(length(pathname) >= 1L, is.character(pathname))
    for (pn in pathname) {
      if (!file_test("-f", pn)) stop("File not found: ", sQuote(pn))
    }
    conf <- sourceConfFiles(pathname)
  } else {
    stopifnot(is.environment(conf))
  }

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            globals=globals,
			    label=label,
                            conf=conf, workers=workers,
                            resources=resources,
			    job.delay=job.delay, ...)
  future$pathname <- pathname
  
  if (!future$lazy) future <- run(future)

  future
}
class(batchjobs_custom) <- c("batchjobs_custom", "batchjobs", "multiprocess", "future", "function")
