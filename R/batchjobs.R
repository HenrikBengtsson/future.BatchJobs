#' A BatchJobs future represents a future that is resolved by one of the many BatchJobs backends
#'
#' @param expr An R expression to be evaluated.
#' @param envir The environment from which global environment
#'              are search from.
#' @param substitute Controls whether \code{expr} should be
#'                   \code{substitute()}:d or not.
#' @param backend The BatchJobs backend to use, cf. \code{\link{backend}()}.
#' @param finalize If TRUE, any underlying registries are
#' deleted when this object is garbage collected, otherwise not.
#' @param ... Additional arguments pass to \code{\link{AsyncTask}()}.
#'
#' @return Returns a BatchJobsAsyncTask object that also is
#' a \link[future]{Future}.
#'
#' @export
batchjobs <- function(expr, envir=parent.frame(), substitute=TRUE, backend=NULL, finalize=getOption("async::finalize", TRUE), ...) {
  if (substitute) expr <- substitute(expr)

  debug <- getOption("async::debug", FALSE)
  if (!debug) options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")

  ## 1. Create BatchJobs registry
  reg <- tempRegistry(backend=backend)
  if (debug) mprint(reg)

  ## 2. Setup future
  future <- AsyncTask(expr=expr, envir=envir, substitute=FALSE, ...)

  ## 3. Register/submit future
  id <- asyncBatchEvalQ(reg, exprs=list(expr), globals=TRUE, envir=envir)
  future$backend <- list(reg=reg, id=id)
  future <- structure(future, class=c("BatchJobsAsyncTask", class(future)))
  if (debug) mprintf("Created %s future #%d\n", class(future)[1], id)

  ## 4. Register finalizer?
  if (finalize) future <- add_finalizer(future)

  ## 5. Launch processing
  submitJobs(reg, ids=id)
  if (debug) mprintf("Launched future #%d\n", id)

  future
}
