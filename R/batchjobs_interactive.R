#' @export
batchjobs_interactive <- function(expr, envir=parent.frame(), substitute=TRUE, ...) {
  if (substitute) expr <- substitute(expr)

  ## 1. Create
  future <- BatchJobsFuture(expr=expr, envir=envir, substitute=FALSE,
                            backend="interactive", ...)

  ## 2. Launch
  future <- run(future)

  future
}
class(batchjobs_interactive) <- c("batchjobs_interactive", "batchjobs", "uniprocess", "future", "function")
