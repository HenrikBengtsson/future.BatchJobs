#' Switch backend to be used for asynchronous processing
#'
#' \emph{This function is defunct.  Please use \code{plan()} with one
#' of the available \code{batchjobs_*} future strategies.}
#'
#' @param what
#'   A \code{character} \code{vector} of preferred backends to be used.
#'   See Section Details below for supported backends.
#'   If \code{NULL} (default), the currently default backend is returned.
#'   If \code{"reset"}, the backend is reset to \code{"default"}.
#'   If \code{"default"}, the default backend according to alias
#'      \code{"default"} is used (see Details).
#'   If \code{"aliases"}, all registered aliases are returned.
#' @param \ldots Named character arguments specifying custom aliases
#'            for character sets of backends.
#' @param quietly If TRUE, messages are suppressed.
#'
#' @return Returns the name of the backend used, or a list of named aliases.
#'
#' @export
#' @keywords internal
backend <- function(what=NULL, ..., quietly=TRUE) {
  .Defunct(msg=sprintf("backend('%s') is defunct and has been replaced by one of several plan(batchjobs_*)", what))
}

makeBatchJobsConf <- function(cluster.functions, ...) {
  getBatchJobsConf <- importBatchJobs("getBatchJobsConf")

  conf <- getBatchJobsConf()

  conf$cluster.functions <- cluster.functions
  conf$mail.start <- "none"
  conf$mail.done <- "none"
  conf$mail.error <- "none"
  conf$db.driver <- "SQLite"
  conf$db.options <- list()
  conf$default.resources <- list()
  conf$debug <- FALSE
  conf$raise.warnings <- FALSE
  conf$staged.queries <- TRUE
  conf$max.concurrent.jobs <- Inf
  conf$fs.timeout <- NA_real_

  ## Sanity check
  stopifnot(is.environment(conf))

  conf
} ## makeBatchJobsConf()
