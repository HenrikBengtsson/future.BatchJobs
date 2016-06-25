#' A BatchJobs future represents a future that is resolved by one of the many BatchJobs backends
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param backend The BatchJobs backend to use.
#' @param workers (optional) Additional specification for the backend
#' workers.  If NULL, the default is used.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @export
batchjobs <- function(expr, envir=parent.frame(), substitute=TRUE, backend=NULL, workers=NULL, ...) {
  if (substitute) expr <- substitute(expr)

  ## Backend explicitly specified?
  if (!is.null(backend)) {
    stopifnot(length(backend) == 1L, is.character(backend))

    if (identical(backend, "local")) {
      .Deprecated(new="plan(batchjobs_local)",
                  old=sprintf("plan(batchjobs, backend='%s')", backend))
      return(batchjobs_local(expr=expr, envir=envir, substitute=FALSE, ...))
    } else if (identical(backend, "interactive")) {
      .Deprecated(new="plan(batchjobs_interactive)",
                  old=sprintf("plan(batchjobs, backend='%s')", backend))
      return(batchjobs_interactive(expr=expr, envir=envir, substitute=FALSE, ...))
    } else if (grepl("^multicore", backend)) {
      if (identical(backend, "multicore")) {
        return(batchjobs_multicore(expr=expr, envir=envir, substitute=FALSE, workers=workers, ...))
      } else if (grepl("^multicore=", backend)) {
        workers <- as.integer(gsub("^multicore=", "", backend))
        .Deprecated(new=sprintf("plan(batchjobs_multicore, workers=%d)", workers),
                    old=sprintf("plan(batchjobs, backend='%s')", backend))
        return(batchjobs_multicore(expr=expr, envir=envir, substitute=FALSE, workers=workers, ...))
      } else if (grepl("^multicore-", backend)) {
        total <- availableCores(constraints="multicore")
        save <- as.integer(gsub("^multicore-", "", backend))
        .Deprecated(new=sprintf("plan(batchjobs_multicore, workers=availableCores()-%d)", save),
                    old=sprintf("plan(batchjobs, backend='%s')", backend))
        workers <- total - save
        return(batchjobs_multicore(expr=expr, envir=envir, substitute=FALSE, workers=workers, ...))
      }
    } else if (identical(backend, ".BatchJobs.R")) {
      .Deprecated(new="plan(batchjobs_custom)")
      return(batchjobs_custom(expr=expr, envir=envir, substitute=FALSE, pathname=NULL, workers=workers, ...))
    } else if (file_test("-f", backend)) {
      .Deprecated(new=sprintf("plan(batchjobs_custom, pathname='%s')", backend),
                  old=sprintf("plan(batchjobs, backend='%s')", backend))
      return(batchjobs_custom(expr=expr, envir=envir, substitute=FALSE, pathname=backend, workers=workers, ...))
    }
  }

  ## 1. Create
  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            backend=backend, workers=workers, ...)

  ## 2. Launch
  future <- run(future)

  future
}
class(batchjobs) <- c("batchjobs", "future", "function")
