#' @importFrom BatchJobs makeClusterFunctionsInteractive
#' @export
batchjobs_interactive <- function(expr, envir=parent.frame(), substitute=TRUE, ...) {
  if (substitute) expr <- substitute(expr)

  cf <- makeClusterFunctionsInteractive()

  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            cluster.functions=cf, ...)

  future <- run(future)

  future
}
class(batchjobs_interactive) <- c("batchjobs_interactive", "batchjobs", "uniprocess", "future", "function")
