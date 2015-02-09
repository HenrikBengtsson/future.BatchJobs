#' @export
#'
#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  regs <- new.env()
  function(prefix="async", ...) {
    id <- tempvar(prefix=prefix, value=NA, envir=regs)
    makeRegistry(id=id, ...)
  }
})
