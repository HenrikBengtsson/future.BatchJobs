#' Gets the number of BatchJobs workers
#'
#' Tries to infer the total number of BatchJobs workers.  This is
#' done using various ad hoc procedures based on code inspection
#' of BatchJobs itself.
#'
#' @param evaluator A future evaluator function.
#' If NULL (default), the current evaluator as returned
#' by \code{\link{plan}()} is used.
#'
#' @return A number in [1,Inf].
#'
#' @aliases nbrOfWorkers.batchjobs_local nbrOfWorkers.batchjobs_interactive nbrOfWorkers.batchjobs_multicore nbrOfWorkers.batchjobs_custom nbrOfWorkers.batchjobs_lsf nbrOfWorkers.batchjobs_openlava nbrOfWorkers.batchjobs_sge nbrOfWorkers.batchjobs_slurm nbrOfWorkers.batchjobs_torque
#' @importFrom future nbrOfWorkers
#' @export
#' @keywords internal
nbrOfWorkers.batchjobs <- function(evaluator) {
  ## Local functions
  getBatchJobsConf <- importBatchJobs("getBatchJobsConf")

  ## 1. Inspect 'backend' argument
  expr <- formals(evaluator)$backend
  backend <- eval(expr)

  ## 2. If not set, look toward backend()
  if (is.null(backend)) {
    backend <- backend()
    if (is.null(backend)) {
      backend("local")
      backend <- backend()
    }
    callBackend <- FALSE
  } else {
    callBackend <- TRUE
  }

  ## Known uni-process backends
  if (backend %in% c("local", "interactive")) return(1L)

  ## Try to infer from the BatchJobs configuration
  workers <- local({
    ## Temporarily, set backend(backend)?
    if (callBackend) {
      obackend <- backend()
      on.exit(backend(obackend))
      backend(backend)
    }

    conf <- getBatchJobsConf()
    cf <- conf$cluster.functions
    env <- environment(cf$submitJob)

    name <- cf$name
    if (is.null(name)) name <- cf$Name
    if (is.null(name)) return(NULL)

    ## Uni-process backends
    if (name %in% c("Local", "Interactive")) return(1L)

    ## Cluster backends (infinite queue available)
    if (name %in% c("Torque", "SLURM", "SGE", "OpenLava", "LSF")) return(Inf)

    ## Multicore processing?
    if (name %in% c("Multicore")) return(env$ncpus)

    ## Ad-hoc SSH cluster
    if (name %in% c("SSH")) {
      n <- length(env$workers)
      if (n == 0L) return(NULL)
      return(n)
    }

    ## Known cluster function
    NULL
  })

  if (is.numeric(workers)) {
    stopifnot(length(workers) == 1, !is.na(workers), workers >= 1)
    return(workers)
  }

  ## If still not known, fall back to the default of the future package
  NextMethod("nbrOfWorkers")
}


#' @export
nbrOfWorkers.batchjobs_custom <- function(evaluator) {
  ## Local functions
  getBatchJobsConf <- importBatchJobs("getBatchJobsConf")

  ## Infer from 'workers' argument
  expr <- formals(evaluator)$workers
  workers <- eval(expr)
  if (!is.null(workers)) {
    stopifnot(length(workers) >= 1)
    if (is.character(workers)) return(length(workers))
    if (is.numeric(workers)) return(prod(workers))

    stop("Invalid data type of 'workers': ", mode(workers))
  }

  ## If still not known, fall back to the default of the future package
  NextMethod("nbrOfWorkers")
}


#' @export
nbrOfWorkers.batchjobs_local <- function(evaluator) 1L

#' @export
nbrOfWorkers.batchjobs_interactive <- function(evaluator) 1L

#' @export
nbrOfWorkers.batchjobs_multicore <- function(evaluator) {
  expr <- formals(evaluator)$workers
  workers <- eval(expr)
  stopifnot(length(workers) == 1, !is.na(workers), workers >= 1, is.finite(workers))
  workers
}


#' @export
nbrOfWorkers.batchjobs_lsf <- function(evaluator) Inf

#' @export
nbrOfWorkers.batchjobs_openlava <- function(evaluator) Inf

#' @export
nbrOfWorkers.batchjobs_sge <- function(evaluator) Inf

#' @export
nbrOfWorkers.batchjobs_slurm <- function(evaluator) Inf

#' @export
nbrOfWorkers.batchjobs_torque <- function(evaluator) Inf
