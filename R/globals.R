# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions to identify and get global objects
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
asFunction <- function(expr, envir=parent.frame(), ...) {
  eval(substitute(function() x, list(x=expr)), envir=envir, ...)
}


tweakExpression <- function(expr) {
  if (!is.language(expr)) return(expr)

  for (ii in seq_along(expr)) {
    # If expr[[ii]] is "missing", ignore the error.  This
    # happens with for instance expressions like x[,1].
    # FIXME: Is there a better way?!? /HB 2014-05-08
    tryCatch({
      exprI <- expr[[ii]]
      op <- exprI[[1]]
      if (!is.symbol(op)) next
      if (op == "%<-%" || op == "%<=%") {
        lhs <- exprI[[2]]
        rhs <- exprI[[3]]
        exprF <- substitute({a <- b; e}, list(a=lhs, b=rhs, e=exprI))
        expr[[ii]] <- exprF
      } else if (op == "%->%" || op == "%=>%") {
        lhs <- exprI[[3]]
        rhs <- exprI[[2]]
        exprI <- substitute({a <- b; e}, list(a=lhs, b=rhs, e=exprI))
        expr[[ii]] <- exprI
      }
    }, error=function(ex) {})
  }
  expr
} # tweakExpression()

findGlobals <- function(expr, envir=parent.frame(), ..., unlist=TRUE) {
  if (is.list(expr)) {
    names <- lapply(expr, FUN=findGlobals, envir=envir, ...)
    if (unlist) {
      names <- unlist(names, use.names=TRUE)
      names <- sort(unique(names))
    }
  } else {
    ## codetools don't see 'a %<-% b' as an assignment and therefore
    ## picks up 'a' as a global variable.  Here we inject 'a <- b'
    ## to fix this.
    expr <- tweakExpression(expr)
    fcn <- asFunction(expr, envir=envir, ...)
    names <- codetools::findGlobals(fcn, merge=TRUE)
  }
  names
}

findBasePkgs <- local({
  pkgs <- NULL
  function() {
    if (length(pkgs) > 0L) return(pkgs)
    data <- installed.packages()
    isBase <- (data[,"Priority"] %in% "base")
    pkgs <<- rownames(data)[isBase]
    pkgs
  }
})

isBasePkgs <- function(pkgs) {
  pkgs %in% findBasePkgs()
}

#' Get all global objects for one or more R expressions
#'
#' @param expr An R expression or a a list of R expressions.
#' @param envir The environment where to search for globals.
#' @param ... Not used.
#' @param primitive If TRUE, primitive globals are returned, otherwise not.
#' @param base If TRUE, globals part of "base" packages are returned, otherwise not.
#' @param unlist If TRUE, a list of unique objects is returned.
#'        If FALSE, a list of \code{length(expr)} sublists.
#'
#' @return A named list of variables.
#'
#' @export
#' @importFrom BatchJobs batchExport batchMap
#' @keywords internal
getGlobals <- function(expr, envir=parent.frame(), ..., primitive=FALSE, base=FALSE, unlist=TRUE) {
  names <- findGlobals(expr, envir=envir, ..., unlist=unlist)
  globals <- lapply(names, FUN=function(names) {
    objs <- lapply(names, FUN=get, envir=envir, inherits=TRUE)
    names(objs) <- names
    ## Drop primitive functions?
    if (!primitive) {
      objs <- objs[!sapply(objs, FUN=is.primitive)]
    }
    ## Drop function in "base" packages?
    if (!base) {
      pkgs <- sapply(objs, FUN=function(x) environmentName(environment(x)))
      objs <- objs[!isBasePkgs(pkgs)]
    }
    objs
  })
  if (unlist) globals <- Reduce(c, globals)
  globals
}
