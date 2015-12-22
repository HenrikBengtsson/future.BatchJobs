asyncPath <- local({
  path <- NULL
  function(absolute=TRUE) {
    if (is.null(path)) {
      id <- basename(tempdir())
      id <- gsub("Rtmp", "", id, fixed=TRUE)
      dir <- sprintf("async_%s", id)
      path <<- file.path(".async", dir)
    }
    if (absolute) path <- file.path(getwd(), path)
    path
  }
})


#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  regs <- new.env()

  function(backend=NULL, prefix="async", path=NULL, file.dir=NULL, ...) {
    id <- tempvar(prefix=prefix, value=NA, envir=regs)
    if (is.null(path)) path <- asyncPath()
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
