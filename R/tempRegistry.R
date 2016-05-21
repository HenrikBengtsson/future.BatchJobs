#' @importFrom R.utils tempvar
#' @importFrom BatchJobs makeRegistry
tempRegistry <- local({
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
