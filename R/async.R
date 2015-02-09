#' Evaluate an expression asynchronously
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which to expression should be evaluated.
#'
#' @return An AsyncResult
#'
#' @export
#' @importFrom R.utils mcat mstr mprint mprintf
#' @importFrom BatchJobs batchMap submitJobs
async <- function(expr, envir=parent.frame()) {
  # Argument 'expr':
  expr <- substitute(expr)

  # Argument 'envir':
  if (!is.environment(envir))
    throw("Argument 'envir' is not a list: ", class(envir)[1L])


  debug <- getOption("async::debug", FALSE)

  if (!debug) {
    options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")
  }

  if (debug) { mcat("Expression:\n"); mprint(expr) }

  ## Create temporary registry
  reg <- tempRegistry()
  if (debug) mprint(reg)

  ## Create job
  id <- batchEval(reg, exprs=list(expr), globals=TRUE, envir=envir)
  if (debug) mprintf("Created job #%d\n", id)

  ## Submit job
  submitJobs(reg, ids=id)
  if (debug) mprintf("Submitted job #%d\n", id)

  AsyncTask(expr, reg=reg, id=id)
} # async()
