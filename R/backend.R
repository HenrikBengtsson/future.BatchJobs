#' Switch backend to be used for asynchronous processing
#'
#' @param what A character vector of preferred backend to be used.
#' @param ... Named character arguments specifying custom aliases
#'            for character sets of backends.
#'
#' @return Returns the name of the backend used.
#'
#' @note The Windows operating system does not support the 'multicore'
#' backend, which then will be ignored.  If explicitly specified, then
#' an informative warning will be given.
#'
#' @export
#' @importFrom parallel detectCores
#' @importFrom BatchJobs makeClusterFunctionsMulticore makeClusterFunctionsLocal makeClusterFunctionsInteractive
#' @importFrom R.utils use
backend <- local({
  aliases = list()
  last = NULL

  function(what=c(".BatchJobs.R", "multicore-1", "multicore", "interactive", "local", "rscript"), ...) {
    ## Attach BatchJobs here, because it will attach itself later
    ## anyways and then it will load its own default settings and
    ## override whatever settings we use here.
    use("BatchJobs")

    ## Imported functions
    ns <- getNamespace("BatchJobs")
    getBatchJobsConf <- get("getBatchJobsConf", mode="function", envir=ns)
    assignConf <- get("assignConf", mode="function", envir=ns)
    readConfs <- get("readConfs", mode="function", envir=ns)


    ## What was the last backend assigned?
    if (is.null(what) || identical(what, NA) || identical(what, "?")) {
      return(last)
    }


    ## Set custom aliases?
    custom <- list(...)
    if (length(custom) > 0L) {
      names <- names(custom)
      if (is.null(names)) {
        stop("Trying to call backend() without named arguments.")
      }
      for (name in names) {
        value <- custom[[name]]
        if (!is.null(value) && !is.character(value)) {
          stop("Backend aliases must be character vectors: ", mode(value))
        }
        aliases[[name]] <<- value
      }
      return(invisible(aliases))
    }


    explicit_what <- !missing(what)
    dropped <- NULL

    ## Expand aliases
    if (length(aliases) > 0L) {
      names <- names(aliases)
      str(aliases)
      what <- lapply(what, FUN=function(what) {
        if (what %in% names) aliases[[what]] else what
      })
      what <- unlist(what, use.names=FALSE)
    }

    ## Is .BatchJobs.R configuration available?
    if (length(what) > 0L && what[1L] == ".BatchJobs.R") {
      if (!hasUserClusterFunctions()) {
        dropped <- c(dropped, what[1L])
        what <- what[-1L]
      }
    }

    ## Multicore processing is not supported on Windows :(
    if (.Platform$OS == "windows") {
      dropped <- c(dropped, grep("^multicore", what, value=TRUE))
      what <- setdiff(what, dropped)
    }

    ## Always fall back to using the 'interactive' configuration
    if (length(what) == 0L) what <- "interactive"

    ## The final choice
    what <- what[1L]

    ## Inform about dropped requests?
    if (length(dropped) > 0L && explicit_what) {
      warning(sprintf("Some of the preferred backends (%s) are either not available or not supported on your operating system ('%s'). Will use the following backend: %s", paste(sQuote(dropped), collapse=", "), .Platform$OS, sQuote(what)))
    }

    if (what == ".BatchJobs.R") {
      readConfs()
      ## Record last used
      last <<- what
      return(what)
    }

    conf <- getBatchJobsConf()
    if (grepl("^multicore", what)) {
      ncpus0 <- detectCores()
      if (grepl("^multicore=", what)) {
        ncpus <- suppressWarnings(as.integer(gsub("^multicore=", "", what)))
        if (!is.finite(ncpus) || ncpus < 1L) {
          stop("Invalid number of cores specified: ", sQuote(what))
        }
        if (ncpus > ncpus0) {
          warning(sprintf("The number of specific cores (%d) is greater than (%d) what is available accoring to parallel::detectCores(). Will still try to use this requested backend: %s", ncpus, ncpus0, sQuote(what)))
        }
      } else {
        ncpus <- ncpus0
        if (ncpus == 1L) {
          warning(sprintf("This system has only a single core (either it's old machine or parallel::detectCores() returns an incorrect value) available for the '%s' backend.", what))
        } else {
          ## Leave one some cores for other things?
          if (grepl("^multicore=", what)) {
            save <- suppressWarnings(as.integer(gsub("^multicore-", "", what)))
            if (!is.finite(save) || save < 0L) {
              stop("Invalid number of cores specified: ", sQuote(what))
            }
            ncpus <- min(1L, ncpus-save)
            if (ncpus == 1L) {
              warning(sprintf("Only 1 core (out of the %d availble on this system) will be used for the '%s' backend.", ncpus0, what))
            }
          }
        }
      }
      conf$cluster.functions = makeClusterFunctionsMulticore(ncpus=ncpus)
    } else if (what == "local") {
      conf$cluster.functions = makeClusterFunctionsLocal()
    } else if (what == "rscript") {
      conf$cluster.functions = makeClusterFunctionsRscript()
    } else if (what == "interactive") {
      conf$cluster.functions = makeClusterFunctionsInteractive()
    } else {
      stop("Unknown backend: ", sQuote(what))
    }

    conf$mail.start = "none"
    conf$mail.done = "none"
    conf$mail.error = "none"
    conf$db.driver = "SQLite"
    conf$db.options = list()
    conf$default.resources = list()
    conf$debug = FALSE
    conf$raise.warnings = FALSE
    conf$staged.queries = TRUE
    conf$max.concurrent.jobs = Inf
    conf$fs.timeout = NA_real_

    ## Use it?
    assignConf(conf)

    ## Record last used
    last <<- what

    what
  }
}) # backend()



#' @importFrom BatchJobs makeClusterFunctions makeSubmitJobResult
makeClusterFunctionsRscript <- function(parallel=FALSE) {
  submitJob = function(conf, reg, job.name, rscript, log.file, job.dir, resources, arrayjobs) {
    system2(command = file.path(R.home("bin"), "Rscript"),
            args = rscript,
            stdout = log.file,
            stderr = log.file,
            wait = !parallel)
    makeSubmitJobResult(status = 0L, batch.job.id = "cfLocal")
  }

  killJob = function(conf, reg, batch.job.id) NULL

  listJobs = function(conf, reg) {
    getJobParentDir <- get("getJobParentDir", mode="function", envir=getNamespace("BatchJobs"))
    fd = reg$file.dir
    jd = getJobParentDir(fd)
    jobdirs = dir(path=jd, pattern="^[0-9]+$", full.names=TRUE)
    ids <- as.integer(basename(jobdirs))
    ids
  }

  getArrayEnvirName = function() NA_character_

  name <- if (parallel) "multirscript" else "rscript"
  makeClusterFunctions(name = name, submitJob = submitJob, killJob = killJob,
                       listJobs = listJobs, getArrayEnvirName = getArrayEnvirName)
}


# Check whether user specifies 'cluster.functions' in one of
# the .BatchJobs.R files that BatchJobs loads.
#
#' @importFrom R.utils mprint
hasUserClusterFunctions <- function(debug=FALSE) {
  ns <- getNamespace("BatchJobs")
  findConfigs <- get("findConfigs", mode="function", envir=ns)
  sourceConfFiles <- get("sourceConfFiles", mode="function", envir=ns)

  pathnames <- findConfigs()
  pathnames <- pathnames[basename(pathnames) != "BatchJobs_global_config.R"]
  if (debug) mprint(pathnames)

  suppressPackageStartupMessages({
    config <- sourceConfFiles(pathnames)
  })

  exists("cluster.functions", mode="list", envir=config)
}

