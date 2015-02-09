#' Delayed asynchroneous assignment
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
  call <- substitute(async(a, envir=b), list(a=expr, b=assign.env))
  env <- new.env()
  env$job <- eval(call, envir=assign.env)
  delayedAssign(name, await(env$job, cleanup=TRUE), eval.env=env, assign.env=assign.env)
}

`%<=%` <- function(x, value) {
  name <- as.character(substitute(x))
  expr <- substitute(value)
  delayedAsyncAssign(name, expr, assign.env=parent.frame(1))
}

`%=>%` <- function(x, value) {
  name <- as.character(substitute(value))
  expr <- substitute(x)
  delayedAsyncAssign(name, expr, assign.env=parent.frame(1))
}

#' Delayed non-asynchroneous assignment
#'
#' @usage x %<-% value
#'
#' @export
`%<-%` <- function(x, value) {
  name <- as.character(substitute(x))
  envir <- parent.frame(1)
  delayedAssign(name, value, assign.env=envir)
}
