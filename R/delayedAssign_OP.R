#' @importFrom listenv get_variable
delayedAssignInternal <- function(target, expr, envir=parent.frame(), substitute=FALSE) {
  target <- parseEnvSubset(target, envir=envir, substitute=substitute)
  assign.env <- target$envir

  name <- target$name
  if (inherits(target$envir, "listenv")) {
    if (target$exists) {
      name <- get_variable(target$envir, name, mustExist=TRUE, create=FALSE)
    } else {
      if (nzchar(name)) {
        name <- get_variable(target$envir, name, mustExist=FALSE, create=TRUE)
      } else if (is.finite(target$idx)) {
        name <- get_variable(target$envir, target$idx, mustExist=FALSE, create=TRUE)
      } else {
        stop("INTERNAL ERROR: Zero length variable name and unknown index.")
      }
    }
  }

  a <- NULL; rm(list="a"); # To please R CMD check
  call <- substitute(local(a), list(a=expr))
  delayedAssign(name, eval(call, envir=envir), assign.env=assign.env)
} # delayedAssignInternal()


#' Delayed synchroneous evaluation ("lazy evaluation")
#'
#' Infix operator for delayed ("lazy") synchroneous evaluation
#' such that the right-hand side (RHS) expression is evaluated
#' if and only if the left-hand side (LHS) object is requested.
#'
#' @usage x %<-% value
#'
#' @seealso Internally \link[base]{delayedAssign}() is used.
#' @aliases %->%
#' @export
#' @export %->%
`%<-%` <- function(x, value) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)
  delayedAssignInternal(target, expr, envir=envir, substitute=FALSE)
}

`%->%` <- function(value, x) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)
  delayedAssignInternal(target, expr, envir=envir, substitute=FALSE)
}
