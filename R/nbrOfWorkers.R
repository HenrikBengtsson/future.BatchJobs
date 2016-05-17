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
#' @importFrom future nbrOfWorkers
#' @export
nbrOfWorkers.batchjobs <- function(evaluator) {
  ## Local functions
  getBatchJobsConf <- function() {
    ns <- getNamespace("BatchJobs")
    getBatchJobsConf <- get("getBatchJobsConf", envir=ns, mode="function")
    getBatchJobsConf()
  }

  ## 1. Inspect 'backend' argument
  expr <- formals(evaluator)$backend
  backend <- eval(expr)

  ## 2. If not set, look toward backend()
  if (is.null(backend)) backend <- backend()

  ## Known uni-process backends
  if (backend %in% c("local", "interactive")) return(1L)

  ## Try to infer from the BatchJobs configuration
  workers <- local({
    ## Make sure to undo
    obackend <- backend()
    on.exit(backend(obackend))

    ## Set backend temporarily
    backend(backend)

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
    stopifnot(length(workers) == 1, is.finite(workers), workers >= 1)
    return(workers)
  }

  ## If still not known, fall back to the default of the future package
  NextMethod("batchjobs")
}

