#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
  ## All known BatchJobs registries
  regs <- new.env()

  makeRegistry <- function(...) {
    ## PROBLEM:
    ## Calling makeRegistry() causes BatchJobs to be attached and
    ## therefore outputs:
    ##   Loading required package: BatchJobs
    ##   Loading required package: BBmisc
    ## as soon as the first BatchJobs future is created.
    ## See also: https://github.com/tudo-r/BatchJobs/issues/68
    ##
    ## WORKAROUND:
    ## In order to avoid the above output message (sent to stderr)
    ## we will attach BatchJobs already here and suppress output.
    suppressPackageStartupMessages(require("BatchJobs"))

    BatchJobs::makeRegistry(...)
  } ## makeRegistry()

  function(label="BatchJobs", path=NULL, ...) {
    ## The job label (the name on the job queue) - could be duplicated
    label <- as.character(label)

    ## This session's path holding all of its future BatchJobs directories
    ##   e.g. .future/<datetimestamp>-<unique_id>/
    if (is.null(path)) path <- futureCachePath()
    
    ## The BatchJobs subfolder for a specific future - must be unique
    prefix <- sprintf("%s_", label)
    unique <- FALSE
    while (!unique) {
      ## The FutureRegistry key for this BatchJobs future - must be unique
      key <- tempvar(prefix=prefix, value=NA, envir=regs)
      ## The directory for this BatchJobs future
      ##   e.g. .future/<datetimestamp>-<unique_id>/<key>/
      pathRegistry <- file.path(path, paste0(key, "-files"))
      ## Should not happen, but just in case.
      unique <- !file.exists(pathRegistry)
    }

    ## If/when makeRegistry() attaches BatchJobs, we need
    ## to prevent it from overriding the configuration
    ## already set.
    oopts <- options(BatchJobs.load.config=FALSE)
    on.exit(options(oopts), add=TRUE)

    makeRegistry(id=label, file.dir=pathRegistry, ...)
  }
})
