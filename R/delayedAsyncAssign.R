#' Delayed asynchroneous evaluation
#'
#' Method and infix operators for delayed assignments while evaluating
#' the statement in the background/in parallel.
#'
#' @param name the name of the variable to assign.
#' @param value the R expression to be asynchroneous evaluated and
#' whose value will be assigned to the variable.
#' @param envir The environment from which global variables used by
#' the expression should be search for.
#' @param assign.env The environment to which the variable should
#' be assigned.
#' @param substitute Controls whether \code{expr} should be
#' \code{substitute()}:d or not.
#'
#' @return A delayed assignment which, when evaluated, will retrieve
#' the value of the asynchronous evaluation.
#'
#' @example incl/delayedAsyncAssign_OP.R
#'
#' @seealso \code{\link{asyncEvalQ}()}
#'
#' @aliases %<=% %=>%
#' @export
#' @export %<=% %=>%
delayedAsyncAssign <- function(name, value, envir=parent.frame(), assign.env=envir, substitute=TRUE) {
  stopifnot(is.character(name), !is.na(name), nzchar(name))
  if (substitute) value <- substitute(value)

  ## Name of "future" (task) to be save in parallel to the
  ## "promise" variable
  future_name <- sprintf(".task_%s", name)
  if (exists(future_name, envir=envir)) {
    msg <- sprintf("A future with name %s already exists in environment %s: %s", sQuote(future_name), sQuote(environmentName(envir)), hpaste(ls(envir=envir, all.names=TRUE)))
##    warning(msg)
  }

  ## Asynchroneously evaluate expression/value as a "future"
  ## and assign its value to a variable as a "promise".
  ## NOTE: We make sure to pass 'envir' in order for globals to
  ## be located properly.
  a <- b <- NULL; rm(list=c("a", "b")) ## To please R CMD check
  call <- substitute(future(a, envir=b), list(a=value, b=envir))
  future <- eval(call, envir=assign.env)

  ## Assign future to assignment environment
  future_without_gc <- future
  future_without_gc$.gcenv <- NULL
  assign(future_name, future_without_gc, envir=assign.env)

  ## Create a promise for the future's value.
  ## Here value may throw an error causing the assign value to be a
  ## "delayed" error, which will be thrown each time the variable is
  ## retrieved.
  env <- new.env()
  env$job <- future
  delayedAssign(name, {
    if (inherits(future, "AsyncTask")) {
      value <- await(future, cleanup=TRUE)
    } else {
      value <- value(future)
    }
    value
  }, eval.env=env, assign.env=assign.env)

  invisible(env)
}
