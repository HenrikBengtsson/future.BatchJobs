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
#' @aliases nbrOfWorkers.batchjobs_interactive nbrOfWorkers.batchjobs_multicore nbrOfWorkers.batchjobs_custom nbrOfWorkers.batchjobs_lsf nbrOfWorkers.batchjobs_openlava nbrOfWorkers.batchjobs_sge nbrOfWorkers.batchjobs_slurm nbrOfWorkers.batchjobs_torque
#' @importFrom future nbrOfWorkers
#' @export
#' @keywords internal
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

  ## If still not known, assume Inf
  Inf
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
