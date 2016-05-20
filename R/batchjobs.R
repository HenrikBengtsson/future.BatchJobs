#' A BatchJobs future represents a future that is resolved by one of the many BatchJobs backends
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param backend The BatchJobs backend to use, cf. \code{\link{backend}()}.
#' @param resources A named list of resources needed by this future.
#' @param \ldots Additional arguments passed to \code{\link{BatchJobsFuture}()}.
#'
#' @return An object of class \code{BatchJobsFuture}.
#'
#' @export
batchjobs <- function(expr, envir=parent.frame(), substitute=TRUE, backend=NULL, resources=list(), ...) {
  if (substitute) expr <- substitute(expr)

  ## Backend explicitly specified?
  if (!is.null(backend)) {
    stopifnot(length(backend) == 1L, is.character(backend))

    if (identical(backend, "local")) {
      return(batchjobs_local(expr=expr, envir=envir, substitute=FALSE, ...))
    } else if (identical(backend, "interactive")) {
      return(batchjobs_interactive(expr=expr, envir=envir, substitute=FALSE, ...))
    } else if (identical(backend, ".BatchJobs.R")) {
      return(batchjobs_conf(expr=expr, envir=envir, substitute=FALSE, pathnames=NULL, ...))
    } else if (grepl("^multicore", backend)) {
        if (identical(backend, "multicore")) {
        return(batchjobs_multicore(expr=expr, envir=envir, substitute=FALSE, ...))
      } else if (grepl("^multicore=", backend)) {
        workers <- as.integer(gsub("^multicore=", "", backend))
        return(batchjobs_multicore(expr=expr, envir=envir, substitute=FALSE, workers=workers, ...))
      } else if (grepl("^multicore-", backend)) {
        total <- availableCores(constraints="multicore")
        save <- as.integer(gsub("^multicore-", "", backend))
        workers <- total - save
        return(batchjobs_multicore(expr=expr, envir=envir, substitute=FALSE, workers=workers, ...))
      }
    } else if (file_test("-f", backend)) {
      return(batchjobs_conf(expr=expr, envir=envir, substitute=FALSE, pathnames=backend, ...))
    }
  }

  ## 1. Create
  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            backend=backend, resources=resources, ...)

  ## 2. Launch
  future <- run(future)

  future
}
class(batchjobs) <- c("batchjobs", "future", "function")
