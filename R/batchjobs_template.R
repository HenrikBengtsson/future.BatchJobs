#' BatchJobs LSF, OpenLava, SGE, Slurm and Torque futures
#'
#' LSF, OpenLava, SGE, Slurm and Torque BatchJobs futures are asynchronous
#' and multiprocess, will not block, and will be evaluated on a compute
#' cluster via a job scheduler.
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param pathname A BatchJobs template file (brew formatted).
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @aliases batchjobs_openlava batchjobs_sge batchjobs_slurm batchjobs_torque
#' @export
batchjobs_lsf <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="lsf", ...)
}
class(batchjobs_lsf) <- c("batchjobs_lsf", "batchjobs", "multiprocess", "future", "function")

#' @export
batchjobs_openlava <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="openlava", ...)
}
class(batchjobs_openlava) <- c("batchjobs_openlava", "batchjobs", "multiprocess", "future", "function")

#' @export
batchjobs_sge <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="sge", ...)
}
class(batchjobs_sge) <- c("batchjobs_sge", "batchjobs", "multiprocess", "future", "function")

#' @export
batchjobs_slurm <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="slurm", ...)
}
class(batchjobs_slurm) <- c("batchjobs_slurm", "batchjobs", "multiprocess", "future", "function")

#' @export
batchjobs_torque <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="torque", ...)
}
class(batchjobs_torque) <- c("batchjobs_torque", "batchjobs", "multiprocess", "future", "function")


#' @importFrom BatchJobs makeClusterFunctionsLSF
#' @importFrom BatchJobs makeClusterFunctionsSGE
#' @importFrom BatchJobs makeClusterFunctionsSLURM
#' @importFrom BatchJobs makeClusterFunctionsTorque
batchjobs_by_template <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, type=c("lsf", "openlava", "sge", "slurm", "torque"), ...) {
  if (substitute) expr <- substitute(expr)
  type <- match.arg(type)

  makeCFs <- switch(type,
    lsf      = makeClusterFunctionsLSF,
    openlava = importBatchJobs("makeClusterFunctionsOpenLava"),
    sge      = makeClusterFunctionsSGE,
    slurm    = makeClusterFunctionsSLURM,
    torque   = makeClusterFunctionsTorque
  )

  ## Search for a default template file?
  if (is.null(pathname)) {
    paths <- c(".", "~", system.file("conf", package="future.BatchJobs"))
    filename <- sprintf("%s.brew", type)
    pathnames <- file.path(paths, filename)
    pathnames <- pathnames[file_test("-f", pathnames)]
    if (length(pathnames) == 0L) {
      stop(sprintf("Failed to locate a %s template file", sQuote(filename)))
    }
    pathname <- pathnames[1]
  }

  cluster.functions <- makeCFs(pathname)

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            cluster.functions=cluster.functions,
                            backend=type, ...)

  future <- run(future)

  future
} ## batchjobs_by_template()
