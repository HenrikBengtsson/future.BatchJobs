# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions that should really be in 'BatchJobs'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' A batch map function for R expressions
#'
#' @param reg A Registry.
#' @param exprs A list of R expressions.
#' @param globals A named list of R objects to be loaded by each job.
#'        If TRUE, globals are automatically searched for.
#' @param envir The environment where to search for globals.
#' @param ... Additional arguments passed to \code{batchMap()}.
#'
#' @return Job IDs.
#'
#' @seealso This function is \pkg{BatchJobs}'s analogy to
#' \code{parallel::clusterEvalQ()} and \code{parallel::clusterExport()}
#' if globals are passed along as well.
#'
#' @export
#' @importFrom BatchJobs batchExport batchMap addRegistryPackages
batchEvalQ <- function(reg, exprs, globals=TRUE, envir=parent.frame(), ...) {
  ## Identify globals?
  if (isTRUE(globals)) {
    globals <- getGlobals(exprs, envir=envir, unlist=TRUE)
  }

  pkgsNeeded <- NULL
  if (is.list(globals) && length(globals) > 0L) {
    ## Scan 'globals' for which packages needs to be loaded.
    ## This information is in the environment name of the objects.
    pkgs <- sapply(globals, FUN=function(obj) {
      environmentName(environment(obj))
    })
    pkgs <- sort(unique(pkgs))
    pkgs <- pkgs[nchar(pkgs) > 0L]
    if (length(pkgs) > 0L) {
      pd <- lapply(pkgs, FUN=packageDescription, encoding=NA)
      # Drop unknown package with a warning (should not happen, but)
      unknown <- pkgs[sapply(pd, FUN=function(x) !is.list(x) && is.na(x))]
      if (length(unknown) > 0L) {
        pd <- pd[!unknown]
        pkgs <- pkgs[!unknown]
        warning("Detected globals in environments/namespaces that refer to unknown packages, which are ignored: ", paste(sQuote(unknown), collapse=", "))
      }
      basePkgs <- sapply(pd, FUN=function(x) {
          !is.null(x$Priority) && x$Priority == "base"
      })
      pkgs <- pkgs[!basePkgs]
    }

    if (length(pkgs) > 0L) {
      addRegistryPackages(reg, packages=pkgs)
    }

    ## BatchJobs::batchExport() validated names of globals using
    ## checkmate::assertList(more.args, names="strict") which doesn't
    ## like names such as "{", although they should be valid indeed.
    keep <- grep("^[.a-zA-Z]", names(globals))
    globals <- globals[keep]

    batchExport(reg, li=globals)
  }
  rm(list=c("globals")) # Not needed anymore

  fun <- function(expr, ..., envir=parent.frame()) {
    eval(expr, envir=envir)
  }
  ids <- batchMap(reg, fun=fun, exprs, ...)
} # batchEvalQ()
