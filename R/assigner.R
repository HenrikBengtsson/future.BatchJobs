# Operator for delayed assignments while evaluating
# the statement in the background/in parallel.
#' @export
#' @importFrom R.utils mprint
`%<-|%` <- function(x, y, envir=parent.frame()) {
  name <- as.character(substitute(x))
  expr <- substitute(y)

  debug <- getOption("async::debug", FALSE)

  env <- new.env()

  call <- substitute(async(.expr., envir=.envir.), list(.expr.=expr, .envir.=envir))
  env$job <- eval(call, envir=envir)

  if (debug) mprint(env$job)

  delayedAssign(name, await(job), eval.env=env, assign.env=envir)
}

#' @export
`%<-||%` <- function(x, y, envir=parent.frame()) NA


# Operator for delayed assignments
#' @export
`%<-%` <- function(x, y, envir=parent.frame()) {
  name <- as.character(substitute(x))
  delayedAssign(name, y, assign.env=envir)
}
