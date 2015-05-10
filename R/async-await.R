#' Explicit asynchroneous evaluation
#'
#' \code{async()} evaluates an expression asynchroneous and
#' \code{await()} retrieves its value.
#'
#' @usage async(expr, envir=parent.frame())
#' @usage await(task)
#'
#' @param expr An R expression to be evaluated
#' @param envir The environment from which global environment
#'              are search from.
#'
#' @return \code{async()} returns an AsyncTask object.
#'
#' @rdname async
#' @export
async <- function(expr, envir=parent.frame()) {
  expr <- substitute(expr)
  BatchJobsAsyncTask(expr=expr, envir=envir, substitute=FALSE)
}

#'
#' @param task An \code{AsyncTask} object whose value to retrieve.
#' @param ... Not used.
#'
#' @return \code{await()} returns the value of the expression.
#'
#' @rdname async
#' @export
await <- function(task, ...) UseMethod("await")
