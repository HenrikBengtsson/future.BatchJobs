# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions to identify and get global objects
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
asFunction <- function(expr, envir=parent.frame(), ...) {
  eval(substitute(function() x, list(x=expr)), envir=envir, ...)
}

findGlobals <- function(expr, envir=parent.frame(), ..., unlist=TRUE) {
  if (is.list(expr)) {
    names <- lapply(expr, FUN=findGlobals, envir=envir, ...)
    if (unlist) {
      names <- unlist(names, use.names=TRUE)
      names <- sort(unique(names))
    }
  } else {
    names <- codetools::findGlobals(asFunction(expr, envir=envir, ...))
    ## checkmate::assertList(more.args, names="strict") don't
    ## like names such as "{", which should be valid indeed.
    names <- grep("^[.a-zA-Z]", names, value=TRUE)
    names <- sort(unique(names))
  }
  names
}

#' Get all global objects for one or more R expressions
#'
#' @param expr An R expression or a a list of R expressions.
#' @param envir The environment where to search for globals.
#' @param ... Not used.
#' @param unlist If TRUE, a list of unique objects is returned.
#'        If FALSE, a list of \code{length(expr)} sublists.
#'
#' @return A named list of variables.
#'
#' @export
#' @importFrom BatchJobs batchExport batchMap
getGlobals <- function(expr, envir=parent.frame(), ..., unlist=TRUE) {
  names <- findGlobals(expr, envir=envir, ..., unlist=unlist)
  globals <- lapply(names, FUN=function(names) {
    objs <- lapply(names, FUN=get, envir=envir, inherits=TRUE)
    names(objs) <- names
    objs
  })
  if (unlist) globals <- Reduce(c, globals)
  globals
}
