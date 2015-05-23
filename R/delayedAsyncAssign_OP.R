#' @importFrom listenv get_variable parse_env_subset
delayedAsyncAssignInternal <- function(target, expr, envir=parent.frame(), substitute=FALSE) {
  target <- parse_env_subset(target, envir=envir, substitute=substitute)
  assign.env <- target$envir
  name <- target$name

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

  delayedAsyncAssign(name, expr, envir=envir, assign.env=assign.env, substitute=FALSE)
} # delayedAsyncAssignInternal()


`%<=%` <- function(x, value) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)
  delayedAsyncAssignInternal(target, expr, envir=envir, substitute=FALSE)
}

`%=>%` <- function(value, x) {
  target <- substitute(x)
  expr <- substitute(value)
  envir <- parent.frame(1)
  delayedAsyncAssignInternal(target, expr, envir=envir, substitute=FALSE)
}
