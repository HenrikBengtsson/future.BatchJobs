#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  regs <- new.env()
  function(prefix="async", file.dir=NULL, ...) {

    id <- tempvar(prefix=prefix, value=NA, envir=regs)
    if (is.null(file.dir)) {
      file.dir <- file.path(getwd(), ".async", paste0(id, "-files"))
    }
    makeRegistry(id=id, file.dir=file.dir, ...)
  }
})
