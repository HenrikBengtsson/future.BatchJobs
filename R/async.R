AsyncTask <- BatchJobsAsyncTask

#' Create an asynchroneous task
#'
#' @param expr The R expression to be evaluated
#' @param envir The environment in which global environment
#' should be located.
#' @param finalize If TRUE, any underlying registries are
#' deleted when this object is garbage collected, otherwise not.
#'
#' @return An AsyncTask object
#'
#' @aliases AsyncTask
#' @export
#' @export AsyncTask
async <- AsyncTask
