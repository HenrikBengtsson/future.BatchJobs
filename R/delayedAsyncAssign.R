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
    msg <- sprintf("A task / future with name %s already exists in environment %s: %s", sQuote(future_name), sQuote(environmentName(envir)), hpaste(ls(envir=envir, all.names=TRUE)))
##    warning(msg)
  }

  ## "Eagerly" start asynchroneous evaluation (task / "future").
  ## NOTE: We make sure to pass 'envir' in order for globals to
  ## be located properly.
  a <- b <- NULL; rm(list=c("a", "b")) ## To please R CMD check
  call <- substitute(async(a, envir=b), list(a=value, b=envir))
  task <- eval(call, envir=assign.env)

  ## Assign task ("future") to assignment environment
  task_without_gc <- task
  task_without_gc$.gcenv <- NULL
  assign(future_name, task_without_gc, envir=assign.env)

  ## Create delayed assignment ("promise") for its result.
  ## Here await may throw an error causing the assign value to be a
  ## "delayed" error, which will be thrown each time the variable is
  ## retrieved.
  env <- new.env()
  env$job <- task
  delayedAssign(name, await(task, cleanup=TRUE),
                      eval.env=env, assign.env=assign.env)

  invisible(env)
}
