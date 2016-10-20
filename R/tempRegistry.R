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

    ## Temporarily disable BatchJobs output?
    ## (i.e. messages and progress bars)
    debug <- getOption("future.debug", FALSE)
    batchjobsOutput <- getOption("future.BatchJobs.output", debug)

    if (!batchjobsOutput) {
      oopts <- options(BatchJobs.verbose=FALSE, BBmisc.ProgressBar.style="off")
      on.exit(options(oopts))
    }

    BatchJobs::makeRegistry(...)
  } ## makeRegistry()

  function(label = "BatchJobs", path=NULL, ...) {
    if (is.null(label)) label <- "BatchJobs"
    ## The job label (the name on the job queue) - may be duplicated
    label <- as.character(label)
    stopifnot(length(label) == 1L, nchar(label) > 0L)
    
    ## This session's path holding all of its future BatchJobs directories
    ##   e.g. .future/<datetimestamp>-<unique_id>/
    if (is.null(path)) path <- futureCachePath()
    
    ## The BatchJobs subfolder for a specific future - must be unique
    prefix <- sprintf("%s_", label)
    
    ## FIXME: We need to make sure 'prefix' consists of only valid
    ## filename characters. /HB 2016-10-19
    prefix <- asValidDirectoryPrefix(prefix)
    
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

    ## FIXME: We need to make sure 'label' consists of only valid
    ## BatchJobs ID characters, i.e. it must match regular
    ## expression "^[a-zA-Z]+[0-9a-zA-Z_]*$".
    ## /HB 2016-10-19
    regId <- asValidRegistryID(label)
    makeRegistry(id=regId, file.dir=pathRegistry, ...)
  }
})



dropNonValidCharacters <- function(name, pattern, default = "BatchJobs") {
  asString <- (length(name) == 1L)
  name <- unlist(strsplit(name, split = "", fixed = TRUE), use.names = FALSE)
  name[!grepl(pattern, name)] <- ""
  if (length(name) == 0L) return(default)
  if (asString) name <- paste(name, collapse = "")
  name
} ## dropNonValidCharacters()

asValidDirectoryPrefix <- function(name) {
  pattern <- "^[-._a-zA-Z0-9]+$"
  ## Nothing to do?
  if (grepl(pattern, name)) return(name)
  name <- unlist(strsplit(name, split = "", fixed = TRUE), use.names = FALSE)
  ## All characters must be letters, digits, underscores, dash, or period.
  name <- dropNonValidCharacters(name, pattern = pattern)
  name <- paste(name, collapse = "")
  stopifnot(grepl(pattern, name))
  name
} ## asValidDirectoryPrefix()

asValidRegistryID <- function(name) {
  pattern <- "^[a-zA-Z]+[0-9a-zA-Z_]*$"
  ## Nothing to do?
  if (grepl(pattern, name)) return(name)

  name <- unlist(strsplit(name, split = "", fixed = TRUE), use.names = FALSE)
  
  ## All characters must be letters, digits, or underscores
  name <- dropNonValidCharacters(name, pattern = "[0-9a-zA-Z_]")
  name <- name[nzchar(name)]

  ## First character must be a letter :/
  if (!grepl("^[a-zA-Z]+", name[1])) name[1] <- "z"

  name <- paste(name, collapse = "")
  
  stopifnot(grepl(pattern, name))
  
  name
} ## asValidRegistryID()
