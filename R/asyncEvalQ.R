#' Evaluate multiple R expressions asynchronously
#'
#' @param exprs A list of R expressions.
#' @param ... Additional arguments passed to \code{asyncBatchEvalQ)}.
#' @param envir The environment from where to search for globals.
#'
#' @return A \code{\link{listenv}} of length \code{length(exprs)}.
#'
#' @export
#' @importFrom R.utils mcat mprint mprintf mstr
#' @importFrom BatchJobs batchExport batchMap addRegistryPackages
#' @keywords internal
asyncEvalQ <- function(exprs, ..., envir=parent.frame()) {
  nexprs <- length(exprs)
  env <- listenv(length=nexprs)
  names(env) <- names(exprs)

  for (ii in seq_len(nexprs)) {
    var <- get_variable(env, ii)
    expr <- exprs[[ii]]
    delayedAsyncAssign(var, expr, assign.env=env)
  }

  env
} # asyncEvalQ()
