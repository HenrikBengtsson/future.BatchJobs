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
  ## Start asynchroneous evaluation ("job")
  call <- substitute(async(a, envir=b), list(a=expr, b=assign.env))
  task <- eval(call, envir=assign.env)
  record(task, name=name)

  ## If an error occurs, we need to be able to troubleshoot it, which
  ## requires access to the 'env$job' object afterward.  This can be
  ## saved "somewhere".  One way is to save it using the name 'name'
  ## and 'envir', e.g. assign(.error_<name>, envir).  This saving
  ## needs to be done by await().
##  target <- list(envir=assign.env, name=name)

  ## Create delayed assignment for its result.
  ## Here await may throw an error causing the assign value to be a
  ## "delayed" error, which will be thrown each time the variable is
  ## retrieved.
  env <- new.env()
  env$job <- task
  delayedAssign(name, await(task, cleanup=TRUE),
                eval.env=env, assign.env=assign.env)
}

.asAssignTarget <- function(expr, envir=parent.frame()) {
  res <- list(envir=envir, name=NULL)

  if (is.symbol(expr)) {
    ## Assignment to variable specified as a symbol
    name <- deparse(expr)
    res$name <- name
  } else {
    n <- length(expr)
    name <- paste(deparse(expr), collapse="")
    if (n != 1L && n != 3L) {
      stop("Not a valid variable name for delayed assignments: ", name, call.=FALSE)
    }

    if (n == 1L) {
      ## Assignment to a variable name
      if (!grepl("^[.a-zA-Z]", name)) {
        stop("Not a valid variable name: ", name, call.=FALSE)
      }
      res$name <- name
    } else if (n == 3L) {
      ## Assignment to enviroment via $ and [[
      op <- expr[[1]]
      if (op == "$" || op == "[[") {
        ## Subset
        idx <- expr[[3]]
        if (is.symbol(idx)) {
          idx <- deparse(idx)
          if (op == "[[") {
            if (!exists(idx, envir=envir, inherits=TRUE)) {
              stop(sprintf("Object %s not found: %s", sQuote(idx), name), call.=FALSE)
            }
            idx <- get(idx, envir=envir, inherits=TRUE)
          }
        }
        if (is.character(idx)) {
        } else {
          stop(sprintf("Invalid subset %s: %s", sQuote(deparse(idx)), name), call.=FALSE)
        }
        res$name <- idx

        ## Target
        objname <- deparse(expr[[2]])
        if (!exists(objname, envir=envir, inherits=TRUE)) {
          stop(sprintf("Object %s not found: %s", sQuote(objname), name), call.=FALSE)
        }
        obj <- get(objname, envir=envir, inherits=TRUE)
        if (is.environment(obj)) {
        } else {
          stop(sprintf("Delayed assignments can not be done to a %s; only to variables and environments: %s", sQuote(mode(obj)), name), call.=FALSE)
        }
        res$envir <- obj
      } else {
        stop("Not a valid target for delayed assignments: ", name, call.=FALSE)
      }
    }
  }

  ## Sanity check
  stopifnot(is.environment(res$envir))
  stopifnot(is.character(res$name))

  res
}

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

#' Evaluate asynchroneous expression on a specific backend
#'
#' @usage x %<-% { expr } %backend% backend
#'
#' @export
#' @importFrom R.utils mprintf
`%backend%` <- function(x, y) {
  lhs <- substitute(x)
  backend <- y
  envir <- parent.frame(1)

  ## Temporary use a different backend
  obackend <- backend(NULL)
  on.exit(backend(obackend, quietly=TRUE))
  what <- backend(backend)
##  mprintf("Using backend: '%s'\n", what)
##  mprintf("Previous backend: '%s'\n", obackend)

  eval(lhs, envir=envir)
}
