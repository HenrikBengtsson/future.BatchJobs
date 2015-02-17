`%<=%` <- function(x, value) {
  envir <- parent.frame(1)
  target <- .asAssignTarget(substitute(x), envir=envir)
  assign.env <- target$envir
  name <- target$name
  expr <- substitute(value)
  delayedAsyncAssign(name, expr, assign.env=assign.env)
}

`%=>%` <- function(x, value) {
  envir <- parent.frame(1)
  target <- .asAssignTarget(substitute(x), envir=envir)
  assign.env <- target$envir
  name <- target$name
  expr <- substitute(x)
  delayedAsyncAssign(name, expr, assign.env=assign.env)
}
