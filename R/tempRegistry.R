#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  regs <- new.env()

  function(backend=NULL, prefix="BatchJobs_", path=NULL, file.dir=NULL, ...) {
    id <- tempvar(prefix=prefix, value=NA, envir=regs)
    if (is.null(path)) path <- futureCachePath()
    if (is.null(file.dir)) file.dir <- file.path(path, paste0(id, "-files"))

    ## Use a non-default backend?
    if (!is.null(backend)) {
      obackend <- backend()
      on.exit(backend(obackend))
      backend(backend)
    }

    ## If/when makeRegistry() attaches BatchJobs, we need
    ## to prevent it from overriding the configuration
    ## already set by backend().
    oopts <- options(BatchJobs.load.config=FALSE)
    on.exit(options(oopts), add=TRUE)

    makeRegistry(id=id, file.dir=file.dir, ...)
  }
})
