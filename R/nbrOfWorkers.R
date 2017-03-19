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
#' @aliases nbrOfWorkers.batchjobs_interactive nbrOfWorkers.batchjobs_multicore nbrOfWorkers.batchjobs
#' @importFrom future nbrOfWorkers
#' @export
#' @keywords internal
nbrOfWorkers.batchjobs <- function(evaluator) {
  ## Infer from 'workers' argument
  expr <- formals(evaluator)$workers
  workers <- eval(expr)
  if (!is.null(workers)) {
    stopifnot(length(workers) >= 1)
    if (is.numeric(workers)) return(prod(workers))
    if (is.character(workers)) return(length(workers))
    stop("Invalid data type of 'workers': ", mode(workers))
  }

  ## If still not known, assume Inf
  Inf
}

#' @export
nbrOfWorkers.batchjobs_multicore <- function(evaluator) {
  expr <- formals(evaluator)$workers
  workers <- eval(expr)
  stopifnot(length(workers) == 1, !is.na(workers), workers >= 1, is.finite(workers))
  workers
}
