#' A batch map function for R expressions
#'
#' @param reg A Registry.
#' @param exprs A list of R expressions.
#' @param globals A named list of R objects to be loaded by each job.
#'        If TRUE, globals are automatically searched for.
#' @param pkgs A character vector of package names to use.
#'        This set will be extended with packages inferred from
#'        the list of globals.
#' @param envir The environment where to search for globals.
#' @param ... Additional arguments passed to \code{batchEvalQ)}.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @export
#' @importFrom globals globalsOf as.Globals packagesOf cleanup
#' @importFrom R.utils hpaste mcat mstr
#' @importFrom BatchJobs batchExport batchMap addRegistryPackages
#' @keywords internal
asyncBatchEvalQ <- function(reg, exprs, globals=TRUE, pkgs=NULL, envir=parent.frame(), ...) {
  debug <- getOption("async::debug", FALSE)

  ## Default maximum export size is 100 MB for now. /HB 2015-04-25
  maxSizeOfGlobals <- getOption("async::maxSizeOfGlobals", 100*1024^2)
  maxSizeOfGlobals <- as.numeric(maxSizeOfGlobals)
  stopifnot(!is.na(maxSizeOfGlobals), maxSizeOfGlobals > 0)


  ## Identify globals?
  if (isTRUE(globals)) {
    globals <- globalsOf(exprs, envir=envir, tweak=tweakExpression, primitive=FALSE, base=FALSE, unlist=TRUE)
    if (debug) {
      mcat("Identified (non-primitive non-\"base\") globals:\n")
      mstr(globals)
    }
  } else if (is.list(globals)) {
    globals <- as.Globals(globals)
  } else if (isFALSE(globals)) {
  } else if (!is.null(globals)) {
    stop("Unknown value on argument 'globals': ", mode(globals))
  }


  if (length(globals) > 0L) {
    ## Append packages associated with globals
    pkgs <- c(pkgs, packagesOf(globals))

    ## Drop all globals which are already part of one of
    ## the packages in 'pkgs'.  They will be available
    ## when those packages are attached.
    pkgsG <- sapply(globals, FUN=function(obj) {
      environmentName(environment(obj))
    })
    keep <- !is.element(pkgsG, pkgs)
    globals <- globals[keep]
    pkgsG <- keep <- NULL ## Not needed anymore

    ## Now drop globals that are primitive functions or
    ## that are part of the base packages, which now are
    ## part of 'pkgs' if needed.
    globals <- cleanup(globals)
  }


  ## Protect against user error exporting too large objects?
  if (length(globals) > 0L && is.finite(maxSizeOfGlobals)) {
    sizes <- lapply(globals, FUN=object.size)
    sizes <- unlist(sizes, use.names=TRUE)
    totalExportSize <- sum(sizes, na.rm=TRUE)
    if (totalExportSize > maxSizeOfGlobals) {
      sizes <- sort(sizes, decreasing=TRUE)
      sizes <- head(sizes, n=3L)
      largest <- sprintf("%s (%g Mb)", sQuote(names(sizes)), sizes/1024^2)
      msg <- sprintf("The total size of all global objects that need to be exported for the asynchronous expression is %g Mb. This exceeds the maximum allowed size of %g Mb (option 'async::maxSizeOfGlobals'). The top largest objects are %s", totalExportSize/1024^2, maxSizeOfGlobals/1024^2, hpaste(largest, lastCollapse=" and "))
      throw(msg)
    }
  }


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Any packages to export?
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Never attach the 'base' package, because that is always
  ## available for all R sessions / implementations.
  pkgs <- setdiff(pkgs, "base")
  if (length(pkgs) > 0L) {
    ## Record which packages in 'pkgs' that are loaded and
    ## which of them are attached (at this point in time).
    isLoaded <- is.element(pkgs, loadedNamespaces())
    isAttached <- is.element(pkgs, attachedPackages())

    ## FIXME: Update the expression such that the new session
    ## will have the same state of (loaded, attached) packages.

    addRegistryPackages(reg, packages=pkgs)
  }
  rm(list=c("pkgs")) # Not needed anymore


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Any globals to export?
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (length(globals) > 0L) {
    ## BatchJobs::batchExport() validated names of globals using
    ## checkmate::assertList(more.args, names="strict") which doesn't
    ## like names such as "{", although they should be valid indeed.
    keep <- grepl("^[.a-zA-Z]", names(globals))
    globals <- globals[keep]
    if (debug && !all(keep)) {
      mcat("Filtered globals:\n")
      mstr(globals)
    }
    if (length(globals) > 0L) batchExport(reg, li=globals)
  }
  rm(list=c("globals")) # Not needed anymore


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Batch process expressions
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  batchEvalQ(reg, exprs=exprs, local=TRUE, ...)
} # asyncBatchEvalQ()
