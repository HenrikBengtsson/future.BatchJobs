# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions that should really be in 'BatchJobs'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' A batch map function for R expressions
#'
#' @param reg A Registry.
#' @param exprs A list of R expressions.
#' @param globals A named list of R objects to be loaded by each job.
#'        If TRUE, globals are automatically searched for.
#' @param envir The environment where to search for globals.
#' @param ... Additional arguments passed to \code{batchMap()}.
#'
#' @return Job IDs.
#'
#' @export
#' @importFrom BatchJobs batchExport batchMap
batchEval <- function(reg, exprs, globals=TRUE, envir=parent.frame(), ...) {
  ## Identify globals?
  if (isTRUE(globals)) {
    globals <- getGlobals(exprs, envir=envir, unlist=TRUE)
  }
  if (is.list(globals) && length(globals) > 0L) {
   batchExport(reg, li=globals)
  }
  rm(list=c("globals")) # Not needed anymore

  fun <- function(expr, ..., envir=parent.frame()) {
    eval(expr, envir=envir)
  }
  ids <- batchMap(reg, fun=fun, exprs, ...)
} # batchEval()
