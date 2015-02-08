#' Delayed asynchroneous assignment
#'
#' Operator for delayed assignments while evaluating
#' the statement in the background/in parallel.
#'
#' @export
#' @importFrom R.utils mprint
`%<-|%` <- function(x, value) {
  name <- as.character(substitute(x))
  expr <- substitute(value)
  envir <- parent.frame(1)

  debug <- getOption("async::debug", FALSE)

  env <- new.env()

  call <- substitute(async(.expr., envir=.envir.), list(.expr.=expr, .envir.=envir))
  env$job <- eval(call, envir=envir)

  if (debug) mprint(env$job)

  delayedAssign(name, await(env$job), eval.env=env, assign.env=envir)
}

#' Void delayed asynchroneous assignment
#'
#' @export
`%<-||%` <- function(x, value) NA

#' Delayed non-asynchroneous assignment
#'
#' @export
`%<-%` <- function(x, value) {
  name <- as.character(substitute(x))
  envir <- parent.frame(1)
  delayedAssign(name, value, assign.env=envir)
}
