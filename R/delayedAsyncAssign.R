#' Delayed asynchroneous evaluation
#'
#' Operator for delayed assignments while evaluating
#' the statement in the background/in parallel.
#'
#' @param name the name of the variable to assign.
#' @param expr the R expression to be asynchroneous evaluated and
#' whose value will be assigned to the variable.
#' @param assign.env The environment to which the variable should
#' be assigned.
#'
#' @return A delayed assignment which, when evaluated, will retrieve
#' the value of the asynchronous evaluation.
#'
#' @seealso \code{\link{async}()}
#'
#' @aliases %<=% %=>%
#' @export
#' @export %<=% %=>%
#' @importFrom R.utils mprint
delayedAsyncAssign <- function(name, expr, assign.env=parent.frame(1)) {
  ## Start asynchroneous evaluation ("job")
  call <- substitute(async(a, envir=b), list(a=expr, b=assign.env))
  task <- eval(call, envir=assign.env)
  record(task, name=name)

  ## If an error occurs, we need to be able to troubleshoot it, which
  ## requires access to the 'env$job' object afterward.  This can be
  ## saved "somewhere".  One way is to save it using the name 'name'
  ## and 'envir', e.g. assign(.error_<name>, envir).  This saving
  ## needs to be done by await().
##  target <- list(envir=assign.env, name=name)

  ## Create delayed assignment for its result.
  ## Here await may throw an error causing the assign value to be a
  ## "delayed" error, which will be thrown each time the variable is
  ## retrieved.
  env <- new.env()
  env$job <- task
  delayedAssign(name, await(task, cleanup=TRUE),
                eval.env=env, assign.env=assign.env)
}
