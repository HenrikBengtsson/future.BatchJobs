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

  ## Function to be run by the parallel evaluator
  fun <- eval(substitute(function(...) {
    .expr.
  }, list(.expr.=expr)))
  if (debug) mprint(list(expr=expr, fun=fun))

  ## Create temporary registry
  reg <- tempRegistry()
  if (debug) mprint(reg)


  ## AD HOC: submitJobs() tried to locate all variables
  ## when saving the function 'fun'.  Here it will also
  ## try to save the variable with name according to 'name',
  ## but won't find one.  So, we create a dummy one here.
  rm(list=c("expr"), inherits=FALSE)


  ## Create job
  id <- batchMap(reg, fun=fun, 0L)
  if (debug) mprintf("Created job #%d\n", id)


  ## Submit job
  submitJobs(reg, ids=id)
  if (debug) mprintf("Submitted job #%d\n", id)


  AsyncResult(reg=reg, id=id)
} # async()
