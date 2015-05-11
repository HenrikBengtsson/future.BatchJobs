#' A batch map function for R expressions
#'
#' @param reg A Registry.
#' @param exprs A list of R expressions.
#' @param globals A named list of R objects to be loaded by each job.
#'        If TRUE, globals are automatically searched for.
#' @param envir The environment where to search for globals.
#' @param ... Additional arguments passed to \code{batchEvalQ)}.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @export
#' @importFrom R.utils mcat mprintf mstr
#' @importFrom BatchJobs batchExport batchMap addRegistryPackages
#' @keywords internal
asyncBatchEvalQ <- function(reg, exprs, globals=TRUE, envir=parent.frame(), ...) {
  debug <- getOption("async::debug", FALSE)

  ## Default maximum export size is 100 MB for now. /HB 2015-04-25
  maxSizeOfGlobals <- getOption("async::maxSizeOfGlobals", 100*1024^2)
  maxSizeOfGlobals <- as.numeric(maxSizeOfGlobals)
  stopifnot(!is.na(maxSizeOfGlobals), maxSizeOfGlobals > 0)


  ## Identify globals?
  if (isTRUE(globals)) {
    globals <- getGlobals(exprs, envir=envir, primitive=FALSE, base=FALSE, unlist=TRUE)
    if (debug) {
      mcat("Identified (non-primitive non-\"base\") globals:\n")
      mstr(globals)
    }
  } else if (list(globals)) {
    globals <- globals[!sapply(globals, FUN=is.primitive)]
  } else {
    stop("Unknown value on argument 'globals': ", mode(globals))
  }

  pkgsNeeded <- NULL
  if (is.list(globals) && length(globals) > 0L) {
    ## Scan 'globals' for which packages needs to be loaded.
    ## This information is in the environment name of the objects.
    pkgs <- sapply(globals, FUN=function(obj) {
      environmentName(environment(obj))
    })

    ## Drop "missing" packages, e.g. globals in globalenv().
    pkgs <- pkgs[nchar(pkgs) > 0L]
    ## Drop global environment
    pkgs <- pkgs[pkgs != "R_GlobalEnv"]
    ## Keep only names matching loaded namespaces
    pkgs <- intersect(pkgs, loadedNamespaces())

    ## Packages to be loaded
    pkgs <- sort(unique(pkgs))
    if (debug) {
      mprintf("Identified %d packages: %s\n", length(pkgs), sQuote(pkgs))
    }

    ## Sanity check
    stopifnot(all(nzchar(pkgs)))

    if (length(pkgs) > 0L) {
      addRegistryPackages(reg, packages=pkgs)
    }

    ## BatchJobs::batchExport() validated names of globals using
    ## checkmate::assertList(more.args, names="strict") which doesn't
    ## like names such as "{", although they should be valid indeed.
    keep <- grepl("^[.a-zA-Z]", names(globals))
    globals <- globals[keep]
    if (debug && !all(keep)) {
      mcat("Filtered globals:\n")
      mstr(globals)
    }

    if (length(globals) > 0L) {
      ## Protect against user error exporting too large objects?
      if (is.finite(maxSizeOfGlobals)) {
        sizes <- lapply(globals, FUN=object.size)
        sizes <- unlist(sizes, use.names=TRUE)
        totalExportSize <- sum(sizes, na.rm=TRUE)
        if (totalExportSize > maxSizeOfGlobals) {
          sizes <- sort(sizes, decreasing=TRUE)
          sizes <- head(sizes, n=3L)
          largest <- sprintf("%s (%g Mb)", sQuote(names(sizes)), sizes/1024^2)
          throw(sprintf("The total size of all global objects that need to be exported for the asynchronous expression is %g Mb. This exceeds the maximum allowed size of %g Mb (option 'async::maxSizeOfGlobals'). The top largest objects are %s", totalExportSize/1024^2, maxSizeOfGlobals/1024^2, hpaste(largest, lastCollapse=" and ")))
        }
      }

      batchExport(reg, li=globals)
    }
  }
  rm(list=c("globals")) # Not needed anymore

  batchEvalQ(reg, exprs=exprs, local=TRUE, ...)
} # asyncBatchEvalQ()
