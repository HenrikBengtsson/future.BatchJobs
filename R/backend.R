#' Switch backend to be used for asynchronous processing
#'
#' @param what
#'   A \code{character} \code{vector} of preferred backends to be used.
#'   See Section Details below for supported backends.
#'   If \code{NULL} (default), the currently default backend is returned.
#'   If \code{"reset"}, the backend is reset to \code{"default"}.
#'   If \code{"default"}, the default backend according to alias
#'      \code{"default"} is used (see Details).
#'   If \code{"aliases"}, all registered aliases are returned.
#' @param \ldots Named character arguments specifying custom aliases
#'            for character sets of backends.
#' @param quietly If TRUE, messages are suppressed.
#'
#' @return Returns the name of the backend used, or a list of named aliases.
#'
#' @details
#' \itemize{
#'  \item \code{".BatchJobs.R"} -
#'    fully configurable batch processing based on a {.BatchJobs.R}
#'    configuration file (searched for in the current directory and
#'    if not found there then in the home directory).
#'    Alternatively, one can specify a specific pathname, e.g.
#'    \code{backend("~/shared/.BatchJobs.R")}.#
#'
#'  \item \code{"interactive"} -
#'    non-parallel processing in the \emph{current} R session (but still
#'    with all calls effectively evaluated as within \code{local()}).
#'    \emph{This backend is always supported.}
#'
#'  \item \code{"local"} -
#'    non-parallel processing in a separate R process.
#'    \emph{This backend is always supported.}
#'    \emph{This is the fallback used if none of the requested
#'          backends are supported.}
#'
#'  \item \code{"multicore"} -
#'    parallel processing using all available cores on the local machine.
#'    The number of available cores is inferred using
#'    \code{\link[future]{availableCores}()}.
#'    Note: Multicore processing is not supported on Windows.
#'    If explicitly specified, then an informative warning will be given,
#'    and it will be ignored.
#'
#'  \item \code{"multicore=<n>"} -
#'    as \code{"multicore"}, but uses (at most) \code{<n>} cores, e.g.
#'    \code{backend("multicore=4")}.
#'  \item \code{"multicore-<d>"} -
#'    as \code{"multicore"}, but uses \code{<d>} less cores, e.g.
#'    \code{backend("multicore-2")} to use all but two cores
#'    (and always at least one).
#' }
#'
#' In addition specify an individual backend, e.g. \code{backend("multicore")},
#' it is also possible to specify a set of possible backends to use, e.g.
#' \code{backend(c(".BatchJobs.R", "multicore"))}.
#' The first supported backend will be used.
#' If none are supported, then the \code{"local"} backend will be used.
#'
#' It is possible to define a custom set of backends using "aliases".
#' For instance, \code{backend(spare=c("multicore-2", "local"))}
#' \emph{defines} the \code{"spare"} backend \emph{set}.  To use this set,
#' do \code{backend("spare")}.  One predefined alias set exists at startup,
#' i.e. \code{backend(default = c(".BatchJobs.R", "multicore-1",
#'            "multicore", "local", "interactive"))}.
#'
#' @export
#' @importFrom future availableCores
#' @importFrom tools file_path_as_absolute
#' @importFrom utils file_test
#' @importFrom BatchJobs makeClusterFunctionsMulticore makeClusterFunctionsLocal makeClusterFunctionsInteractive
backend <- local({
  aliases = list(
    default = c(".BatchJobs.R", "multicore-1", "multicore",
                "local", "interactive")
  )
  last = NULL

  ## WORKAROUND: See comment below.
  dyld_envs <- list()

  function(what=NULL, ..., quietly=TRUE) {
    ## Imported private functions from BatchJobs
    ns <- getNamespace("BatchJobs")
    getBatchJobsConf <- get("getBatchJobsConf", mode="function", envir=ns)
    sourceConfFiles <- get("sourceConfFiles", mode="function", envir=ns)
    assignConf <- get("assignConf", mode="function", envir=ns)
    readConfs <- get("readConfs", mode="function", envir=ns)

    debug <- getOption("future.debug", FALSE)

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

    if (is.null(what)) {
      return(last)
    } else if (identical(what, "aliases")) {
      return(aliases)
    } else if (identical(what, "reset")) {
      what <- "default"
    }

    explicit_what <- !missing(what)
    dropped <- NULL

    ## Expand aliases
    if (length(aliases) > 0L) {
      names <- names(aliases)
      what <- lapply(what, FUN=function(what) {
        if (what %in% names) aliases[[what]] else what
      })
      what <- unlist(what, use.names=FALSE)
    }

    ## BatchJobs configuration?
    if (length(what) >= 1L) {
      ## A "global" or a specific config file?
      ## NOTE: For a file in the current directory, use ./.BatchJobs.R
      if (what[1L] == ".BatchJobs.R") {
        if (!hasUserClusterFunctions()) {
          dropped <- c(dropped, what[1L])
          what <- what[-1L]
        }
      } else if (file_test("-f", what[1L])) {
        pathname <- file_path_as_absolute(what[1L])
        if (!hasUserClusterFunctions(pathname)) {
          stop("The specified file does not specify BatchJobs cluster functions: ", what[1L])
        }
      }
    }

    ## Multicore processing is not supported on Windows :(
    if (.Platform$OS.type == "windows") {
      dropped <- c(dropped, grep("^multicore", what, value=TRUE))
      what <- setdiff(what, dropped)
    }

    ## Always fall back to using the 'local' configuration
    if (length(what) == 0L) what <- "local"

    ## The final choice
    what <- what[1L]

    if (debug) mprintf("backend(): what='%s'\n", what)

    ## Inform about dropped requests?
    if (length(dropped) > 0L && explicit_what && getOption("future.on_unkown_backend", "ignore") == "warn") {
      warning(sprintf("Some of the preferred backends (%s) are either not available or not supported on your operating system ('%s'). Will use the following backend: %s", paste(sQuote(dropped), collapse=", "), .Platform$OS.type, sQuote(what)))
    }

    ## Load specific or global BatchJobs config file?
    if (file_test("-f", what)) {
      if (debug) mprintf("backend(): file='%s'\n", what)
      conf <- sourceConfFiles(what)
      if (debug) {
        mprintf("Setting BatchJobs configuration:\n")
        mstr(as.list(conf))
      }
      assignConf(conf)
      ## Record last used
      last <<- what
      return(what)
    } else if (what == ".BatchJobs.R") {
      if (debug) mprintf("backend(): First available '.BatchJobs.R'\n")
      if (quietly) {
        suppressPackageStartupMessages(readConfs())
      } else {
        readConfs()
      }
      ## Record last used
      last <<- what
      return(what)
    }

    if (debug) mprintf("backend(): Finding action for what='%s'\n", what)

    conf <- getBatchJobsConf()
    if (grepl("^multicore", what)) {
      ncpus0 <- availableCores()
      if (grepl("^multicore=", what)) {
        ncpus <- suppressWarnings(as.integer(gsub("^multicore=", "", what)))
        if (!is.finite(ncpus) || ncpus < 1L) {
          stop("Invalid number of cores specified: ", sQuote(what))
        }
        if (ncpus > ncpus0) {
          warning(sprintf("The number of specific cores (%d) is greater than (%d) what is available accoring to availableCores(). Will still try to use this requested backend: %s", ncpus, ncpus0, sQuote(what)))
        }
      } else {
        ncpus <- ncpus0
        if (ncpus == 1L) {
          warning(sprintf("This system has only a single core (either it's old machine or availableCores() returns an incorrect value) available for the '%s' backend.", what))
        } else {
          ## Leave some cores for other things?
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

      ## WORKAROUND:
      ## On some OS X systems, a system call to 'ps' may output an error message
      ## "dyld: DYLD_ environment variables being ignored because main executable
      ##  (/bin/ps) is setuid or setgid" to standard error that is picked up by
      ## BatchJobs which incorrectly tries to parse it.  By unsetting all DYLD_*
      ## environment variables, we avoid this message.  For more info, see:
      ## * https://github.com/tudo-r/BatchJobs/issues/117
      ## * https://github.com/HenrikBengtsson/future.BatchJobs/issues/59
      ## /HB 2016-05-07
      dyld_envs <<- tryCatch({
        envs <- list()
        res <- system2("ps", stdout=TRUE, stderr=TRUE)
        if (any(grepl("DYLD_", res))) {
          envs <- Sys.getenv()
          envs <- envs[grepl("^DYLD_", names(envs))]
          if (length(envs) > 0L) lapply(names(envs), FUN=Sys.unsetenv)
        }
        envs
      }, error = function(ex) list())

      if (debug) mprintf("backend(): makeClusterFunctionsMulticore(ncpus=%d)\n", ncpus)
      conf$cluster.functions = makeClusterFunctionsMulticore(ncpus=ncpus)
    } else if (what == "local") {
      if (debug) mprintf("backend(): makeClusterFunctionsLocal()\n")
      conf$cluster.functions = makeClusterFunctionsLocal()
    } else if (what == "interactive") {
      conf$cluster.functions = makeClusterFunctionsInteractive()
    } else {
      stop("Unknown backend: ", sQuote(what))
    }


    ## WORKAROUND: Undo above multicore OS X workaround, iff ever done.
    if (!grepl("^multicore", what) && length(dyld_envs) > 0L) {
      do.call(Sys.setenv, args=as.list(dyld_envs))
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

    if (debug) {
      mprintf("Setting BatchJobs configuration:\n")
      mstr(as.list(conf))
    }

    ## Use it!
    assignConf(conf)

    ## Record last used
    last <<- what

    what
  }
}) # backend()


# Check whether user specifies 'cluster.functions' in one of
# the .BatchJobs.R files that BatchJobs loads.
hasUserClusterFunctions <- function(pathnames=NULL, debug=FALSE) {
  ns <- getNamespace("BatchJobs")
  findConfigs <- get("findConfigs", mode="function", envir=ns)
  sourceConfFiles <- get("sourceConfFiles", mode="function", envir=ns)

  if (is.null(pathnames)) {
    pathnames <- findConfigs()
    ## Drop ... ? Where does that file exists? /HB 2015-05-19
    pathnames <- pathnames[basename(pathnames) != "BatchJobs_global_config.R"]
  }
  if (debug) mprint(pathnames)

  suppressPackageStartupMessages({
    config <- sourceConfFiles(pathnames)
  })

  exists("cluster.functions", mode="list", envir=config)
}
