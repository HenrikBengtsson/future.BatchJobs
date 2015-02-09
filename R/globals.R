# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions to identify and get global objects
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
asFunction <- function(expr, envir=parent.frame(), ...) {
  eval(substitute(function() x, list(x=expr)), envir=envir, ...)
}

findGlobals <- function(expr, envir=parent.frame(), ..., unlist=TRUE) {
  if (is.list(expr)) {
    names <- lapply(expr, FUN=findGlobals, envir=envir, ...)

    ## Did any of the expressions need async?
    needsAsync <- any(sapply(names, FUN=function(names) {
      isTRUE(attr(names, "needsAsync"))
    }))

    if (unlist) {
      names <- unlist(names, use.names=TRUE)
      names <- sort(unique(names))
    }
  } else {
    ## FIXME: codetools don't see 'a %<-% { ... }' as an assignment
    ## and is therefore picking up 'a' as a global variable, which
    ## will later cause problems for getGlobals().
    ## Idea: Could we replace all '%<-%' with regular '<-' before
    ## calling codetools?  /HB 2015-02-09
    names <- codetools::findGlobals(asFunction(expr, envir=envir, ...))

    ## Detect whether nested '%<=%' are used or not.  If they
    ## are, we need to load 'async' package in the nested call,
    ## otherwise we don't have to.
    needsAsync <- any(c("%<=%", "%<-%", "%=>%") %in% names)

    ## checkmate::assertList(more.args, names="strict") don't
    ## like names such as "{", which should be valid indeed.
    names <- grep("^[.a-zA-Z]", names, value=TRUE)
    names <- sort(unique(names))
  }

  ## Indicates whether 'async' package is needed or not
  attr(names, "needsAsync") <- needsAsync

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

  ## Indicates whether 'async' package is needed or not
  attr(globals, "needsPkgs") <- c("async")

  globals
}
