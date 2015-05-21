#' Delayed synchroneous evaluation ("lazy evaluation")
#'
#' Infix operator for delayed ("lazy") synchroneous evaluation
#' such that the right-hand side (RHS) expression is evaluated
#' if and only if the left-hand side (LHS) object is requested.
#'
#' @usage x %<-% value
#'
#' @seealso Internally \link[base]{delayedAssign}() is used.
#' @export
#' @aliases %->%
#' @export %->%
`%<-%` <- function(x, value) {
  envir <- parent.frame(1)
  target <- .asAssignTarget(substitute(x), envir=envir)
  assign.env <- target$envir
  name <- target$name
  expr <- substitute(value)
  call <- substitute(local(a), list(a=expr))
  delayedAssign(name, eval(call, envir=envir), assign.env=assign.env)
}

`%->%` <- function(value, x) {
  envir <- parent.frame(1)
  target <- .asAssignTarget(substitute(x), envir=envir)
  assign.env <- target$envir
  name <- target$name
  expr <- substitute(value)
  call <- substitute(local(a), list(a=expr))
  delayedAssign(name, eval(call, envir=envir), assign.env=assign.env)
}
