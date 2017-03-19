#' @inheritParams BatchJobsFuture
#'
#' @importFrom BatchJobs makeClusterFunctionsInteractive
#' @export
batchjobs_interactive <- function(expr, envir=parent.frame(), substitute=TRUE, globals=TRUE, label="BatchJobs", workers=1L, job.delay=FALSE, ...) {
  if (substitute) expr <- substitute(expr)

  cf <- makeClusterFunctionsInteractive()

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            globals=globals,
			    label=label,
			    cluster.functions=cf,
                            workers=workers,
			    job.delay=job.delay, ...)

  if (!future$lazy) future <- run(future)

  future
}
class(batchjobs_interactive) <- c("batchjobs_interactive", "batchjobs", "uniprocess", "future", "function")
