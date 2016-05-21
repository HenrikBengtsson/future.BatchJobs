#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  regs <- new.env()

  function(prefix="BatchJobs_", path=NULL, file.dir=NULL, ...) {
    id <- tempvar(prefix=prefix, value=NA, envir=regs)
    if (is.null(path)) path <- futureCachePath()
    if (is.null(file.dir)) file.dir <- file.path(path, paste0(id, "-files"))

    ## If/when makeRegistry() attaches BatchJobs, we need
    ## to prevent it from overriding the configuration
    ## already set.
    oopts <- options(BatchJobs.load.config=FALSE)
    on.exit(options(oopts), add=TRUE)

    makeRegistry(id=id, file.dir=file.dir, ...)
  }
})
