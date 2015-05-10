#' Delayed asynchroneous evaluation
#'
#' Method and infix operators for delayed assignments while evaluating
#' the statement in the background/in parallel.
#'
#' @param name the name of the variable to assign.
#' @param value the R expression to be asynchroneous evaluated and
#' whose value will be assigned to the variable.
#' @param envir The environment from which global variables used by
#' the expression should be search for.
#' @param assign.env The environment to which the variable should
#' be assigned.
#' @param substitute Controls whether \code{expr} should be
#' \code{substitute()}:d or not.
#'
#' @return A delayed assignment which, when evaluated, will retrieve
#' the value of the asynchronous evaluation.
#'
#' @example inst/delayedAsyncAssign_OP.R
#'
#' @seealso \code{\link{asyncEvalQ}()}
#'
#' @aliases %<=% %=>%
#' @export
#' @export %<=% %=>%
delayedAsyncAssign <- function(name, value, envir=parent.frame(), assign.env=envir, substitute=TRUE) {
  if (substitute) value <- substitute(value)

  ## Start asynchroneous evaluation ("job").  Make sure to pass 'envir'
  ## in order for globals to be located properly.
  a <- b <- NULL; rm(list=c("a", "b")) ## To please R CMD check
  call <- substitute(async(a, envir=b), list(a=value, b=envir))
  task <- eval(call, envir=assign.env)
  record(task, name=name)

  ## Create delayed assignment for its result.
  ## Here await may throw an error causing the assign value to be a
  ## "delayed" error, which will be thrown each time the variable is
  ## retrieved.
  env <- new.env()
  env$job <- task
  delayedAssign(name, await(task, cleanup=TRUE),
                eval.env=env, assign.env=assign.env)
}
