#' BatchJobs LSF, OpenLava, SGE, Slurm and Torque futures
#'
#' LSF, OpenLava, SGE, Slurm and Torque BatchJobs futures are
#' asynchronous multiprocess futures that will be evaluated on
#' a compute cluster via a job scheduler.
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param pathname A BatchJobs template file (\pkg{brew} formatted).
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @details
#' These type of BatchJobs futures rely on BatchJobs backends set
#' up using the following \pkg{BatchJobs} functions:
#' \itemize{
#'  \item \code{\link[BatchJobs]{makeClusterFunctionsLSF}()} for \href{https://en.wikipedia.org/wiki/Platform_LSF}{Load Sharing Facility (LSF)}
#'  \item \code{makeClusterFunctionsOpenLava()} for \href{https://en.wikipedia.org/wiki/OpenLava}{OpenLava} (only BatchJobs (>= 1.7.0))
#'  \item \code{\link[BatchJobs]{makeClusterFunctionsSGE}()} for \href{https://en.wikipedia.org/wiki/Oracle_Grid_Engine}{Sun/Oracle Grid Engine (SGE)}
#'  \item \code{\link[BatchJobs]{makeClusterFunctionsSLURM}()} for \href{https://en.wikipedia.org/wiki/Slurm_Workload_Manager}{Slurm}
#'  \item \code{\link[BatchJobs]{makeClusterFunctionsTorque}()} for \href{https://en.wikipedia.org/wiki/TORQUE}{TORQUE} / PBS
#' }
#'
#' @export
#' @rdname batchjobs_template
#' @name batchjobs_template
batchjobs_lsf <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="lsf", ...)
}
class(batchjobs_lsf) <- c("batchjobs_lsf", "batchjobs", "multiprocess", "future", "function")

#' @export
#' @rdname batchjobs_template
batchjobs_openlava <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="openlava", ...)
}
class(batchjobs_openlava) <- c("batchjobs_openlava", "batchjobs", "multiprocess", "future", "function")

#' @export
#' @rdname batchjobs_template
batchjobs_sge <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="sge", ...)
}
class(batchjobs_sge) <- c("batchjobs_sge", "batchjobs", "multiprocess", "future", "function")

#' @export
#' @rdname batchjobs_template
batchjobs_slurm <- function(expr, envir=parent.frame(), substitute=TRUE, pathname=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  batchjobs_by_template(expr, envir=envir, substitute=FALSE, pathname=pathname, type="slurm", ...)
}
class(batchjobs_slurm) <- c("batchjobs_slurm", "batchjobs", "multiprocess", "future", "function")

#' @export
#' @rdname batchjobs_template
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
    paths <- c(".", "~")
    filename <- sprintf(".BatchJobs.%s.brew", type)
    pathnames <- file.path(paths, filename)

    ## Because R CMD check complains about periods in package files
    path <- system.file("conf", package="future.BatchJobs")
    filename <- sprintf("BatchJobs.%s.brew", type)
    pathname <- file.path(path, filename)
    
    pathnames <- c(pathnames, pathname)
    pathnames <- pathnames[file_test("-f", pathnames)]
    if (length(pathnames) == 0L) {
      stop(sprintf("Failed to locate a %s template file", sQuote(filename)))
    }
    pathname <- pathnames[1]
  }

  cluster.functions <- makeCFs(pathname)

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            cluster.functions=cluster.functions, ...)

  future <- run(future)

  future
} ## batchjobs_by_template()
