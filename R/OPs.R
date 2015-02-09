#' Delayed asynchroneous assignment
#'
#' Operator for delayed assignments while evaluating
#' the statement in the background/in parallel.
#'
#' @param x the symbol to which the value should be assign.
#' @param value the R expression to be asynchroneous evaluated.
#'
#' @return A delayed assignment.
#'
#' @seealso \code{\link{async}()}
#'
#' @export
#' @export %<=% %=>% %<-||%
#' @importFrom R.utils mprint
#'
delayedAsyncAssign <- function(x, value) {
  name <- as.character(substitute(x))
  expr <- substitute(value)
  envir <- parent.frame(1)
  call <- substitute(async(.expr., envir=.envir.), list(.expr.=expr, .envir.=envir))
  env <- new.env()
  env$job <- eval(call, envir=envir)
  delayedAssign(name, await(env$job, cleanup=TRUE), eval.env=env, assign.env=envir)
}
`%<=%` <- delayedAsyncAssign
`%<-||%` <- delayedAsyncAssign

delayedAsyncAssignRev <- function(x, value) {
  name <- as.character(substitute(value))
  expr <- substitute(x)
  envir <- parent.frame(1)
  call <- substitute(async(.expr., envir=.envir.), list(.expr.=expr, .envir.=envir))
  env <- new.env()
  env$job <- eval(call, envir=envir)
  delayedAssign(name, await(env$job, cleanup=TRUE), eval.env=env, assign.env=envir)
}
`%=>%` <- delayedAsyncAssignRev


#' Parallel evaluation of expressions
#'
#' \examples{
#' ## { x <- 1 } %&&% { y <- 1 }
#' }
#'
#' @export
#' @importFrom R.utils mprint
`%&&%` <- function(x, y) {
  exprA <- substitute(x)
  exprB <- substitute(y)
  mprint(exprA)
  mprint(exprB)
  px <- async(exprA)
  py <- async(exprB)
  print(px)
  print(py)
  px & py
}


#' Void delayed asynchroneous assignment
#'
#' @export
`%<-|%` <- function(x, value) NA

#' Delayed non-asynchroneous assignment
#'
#' @export
`%<-%` <- function(x, value) {
  name <- as.character(substitute(x))
  envir <- parent.frame(1)
  delayedAssign(name, value, assign.env=envir)
}
