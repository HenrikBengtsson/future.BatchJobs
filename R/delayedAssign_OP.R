#' Delayed synchroneous evaluation
#'
#' @usage x %<-% value
#'
#' @export
`%<-%` <- function(x, value) {
  envir <- parent.frame(1)
  target <- .asAssignTarget(substitute(x), envir=envir)
  assign.env <- target$envir
  name <- target$name
  expr <- substitute(value)
  call <- substitute(local(a), list(a=expr))
  delayedAssign(name, eval(call, envir=envir), assign.env=assign.env)
}
