#' Get number of available cores on current machine
#'
#' @param sources A character vector specifying which sources to use
#'    to infer the number of available cores (and in what order).
#'
#' @return Return a positive integer equal or greater to one.
#'
#' @details
#' The following sources for inferring the number of cores are supported:
#' \itemize{
#'  \item \code{"PBS"} -
#'    Query Torque/PBS environment variable \code{PBS_NUM_PPN}.
#'  \item \code{"system"} -
#'    Query \code{\link[parallel]{detectCores}()}.
#' }
#'
#' @seealso \code{\link[parallel]{detectCores}()}
#'
#' @export
#' @importFrom parallel detectCores
availableCores <- function(sources=c("PBS", "system")) {
  for (source in sources) {
    ncores <- NA_integer_
    if (source == "PBS") {
      ## Number of cores assigned by Torque/PBS?
      ncores <- as.integer(Sys.getenv("PBS_NUM_PPN", NA))
      if (is.finite(ncores) && ncores > 0L) break
    } else if (source == "system") {
      ## Number of cores available according to parallel::detectCores()
      ncores <- detectCores()
      if (is.finite(ncores) && ncores > 0L) break
    }
  }

  ## The default is to use a single core
  if (is.na(ncores)) ncores <- 1L

  ncores
}
