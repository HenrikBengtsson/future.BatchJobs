#' Create an asynchroneous task
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment from which global environment
#'              are search from.
#'
#' @return An AsyncTask object
#'
#' @export
async <- function(expr, envir=parent.frame()) {
  expr <- substitute(expr)
  BatchJobsAsyncTask(expr=expr, envir=envir, substitute=FALSE)
}
