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
  call <- substitute(async(a, envir=b), list(a=expr, b=assign.env))
  env <- new.env()
  env$job <- eval(call, envir=assign.env)
  delayedAssign(name, await(env$job, cleanup=TRUE), eval.env=env, assign.env=assign.env)
}

.asAssignName <- function(name) {
  if (is.symbol(name)) {
    name <- deparse(name)
  } else {
    n <- length(name)
    name <- paste(deparse(name), collapse="")
    if (n == 0L) {
      stop("Not a valid variable name: ", name, call.=FALSE)
    } else if (n > 1L) {
      stop("Not a valid variable name for delayed assignments: ", name, call.=FALSE)
    }
    str(name)
    if (!grepl("^[.a-zA-Z]", name)) {
      stop("Not a valid variable name: ", name, call.=FALSE)
    }
  }
  name
}

`%<=%` <- function(x, value) {
  name <- substitute(x)
  name <- .asAssignName(name)
  expr <- substitute(value)
  delayedAsyncAssign(name, expr, assign.env=parent.frame(1))
}

`%=>%` <- function(x, value) {
  name <- substitute(x)
  name <- .asAssignName(name)
  expr <- substitute(x)
  delayedAsyncAssign(name, expr, assign.env=parent.frame(1))
}

#' Delayed synchroneous evaluation
#'
#' @usage x %<-% value
#'
#' @export
`%<-%` <- function(x, value) {
  name <- substitute(x)
  name <- .asAssignName(name)
  expr <- substitute(value)
  call <- substitute(local(a), list(a=expr))
  envir <- parent.frame(1)
  delayedAssign(name, eval(call, envir=envir), assign.env=envir)
}

#' @export
#' @importFrom R.utils mprintf
`%backend%` <- function(x, y) {
  lhs <- substitute(x)
  backend <- y
  envir <- parent.frame(1)

  ## Temporary use a different backend
  obackend <- backend("?")
  on.exit(backend(obackend, quietly=TRUE))
  what <- backend(backend)
##  mprintf("Using backend: '%s'\n", what)
##  mprintf("Previous backend: '%s'\n", obackend)

  eval(lhs, envir=envir)
}

