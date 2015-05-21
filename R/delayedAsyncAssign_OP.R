`%<=%` <- function(x, value) {
  expr <- substitute(value)

  envir <- parent.frame(1)
  target <- .asAssignTarget(substitute(x), envir=envir)
  assign.env <- target$envir
  name <- target$name

  delayedAsyncAssign(name, expr, envir=parent.frame(), assign.env=assign.env, substitute=FALSE)
}

`%=>%` <- function(value, x) {
  expr <- substitute(value)

  envir <- parent.frame(1)
  target <- .asAssignTarget(substitute(x), envir=envir)
  assign.env <- target$envir
  name <- target$name

  delayedAsyncAssign(name, expr, envir=parent.frame(), assign.env=assign.env, substitute=FALSE)
}
