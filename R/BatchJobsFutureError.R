#' FutureError class for errors related to BatchJobsFuture:s
#'
#' @param \ldots Arguments passed to \code{\link[future]{FutureError}}
#'
#' @export
#' @importFrom future FutureError
#'
#' @keywords internal
BatchJobsFutureError <- function(...) {
  error <- FutureError(...)
  class(error) <- c("BatchJobsFutureError", class(error))
  error
}
