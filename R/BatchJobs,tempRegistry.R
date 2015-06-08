#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  regs <- new.env()
  function(prefix="async", file.dir=NULL, ...) {
    id <- tempvar(prefix=prefix, value=NA, envir=regs)
    if (is.null(file.dir)) {
      file.dir <- file.path(getwd(), ".async", paste0(id, "-files"))
    }

    ## If/when makeRegistry() attaches BatchJobs, we need
    ## to prevent it from overriding the configuration
    ## already set by backend().
    oopts <- options(BatchJobs.load.config=FALSE)
    on.exit(options(oopts))
    makeRegistry(id=id, file.dir=file.dir, ...)
  }
})
