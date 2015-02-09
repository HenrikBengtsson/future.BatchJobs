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
    R.utils::withSink(str(list(...)), file="T:/foo.txt")
    .expr.
  }, list(.expr.=expr)))
  if (debug) str(list(expr=expr, fun=fun))


  ## Identify globals of fun(), i.e. objects that
  ## fun() needs but are not assigned locally.  They
  ## may be availble globally
  globalNames <- codetools::findGlobals(fun)
  if (debug) str(list(expr=expr, fun=fun, globalsNames=globalNames))

  ## Get all globals
  globals <- lapply(globalNames, FUN=get, envir=envir, inherits=TRUE)
  names(globals) <- globalNames
  if (debug) str(list(expr=expr, fun=fun, globals=globals))

  if (debug) {
    ## DEBUG: Check if the function can be evaluated
#    ttt <- fun()
#    mcat("Function fun() evaluated.\n")
#    mprint(ttt)
  }

  ## Create temporary registry
  reg <- tempRegistry()
  if (debug) mprint(reg)

  ## AD HOC: submitJobs() tried to locate all variables
  ## when saving the function 'fun'.  Here it will also
  ## try to save the variable with name according to 'name',
  ## but won't find one.  So, we create a dummy one here.
#  rm(list=c("expr"), inherits=FALSE)

  batchExport(reg, ll=globals)

  ## Create job
  id <- batchMap(reg, fun=fun, 0L, more.args=list(a=1))
  if (debug) mprintf("Created job #%d\n", id)


  ## Submit job
  submitJobs(reg, ids=id)
  if (debug) mprintf("Submitted job #%d\n", id)

  AsyncTask(expr, reg=reg, id=id)
} # async()
