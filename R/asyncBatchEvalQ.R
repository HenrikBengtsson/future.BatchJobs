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
#' @param \ldots Additional arguments passed to \code{batchEvalQ)}.
#'
#' @return Vector of type \code{integer} with job ids.
#'
#' @export
#' @importFrom utils object.size head
#' @importFrom globals globalsOf as.Globals packagesOf cleanup
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
    ns <- getNamespace("future")
    tweakExpression <- get("tweakExpression", envir=ns, mode="function")
    globals <- globalsOf(exprs, envir=envir, tweak=tweakExpression, dotdotdot="return", primitive=FALSE, base=FALSE, unlist=TRUE)
    if (debug) {
      mcat("Identified (non-primitive non-\"base\") globals:\n")
      mstr(globals)
    }

    ## Tweak expression to be called with global ... arguments?
    if (inherits(globals$`...`, "DotDotDotList")) {
      ## Missing global '...'?
      if (!is.list(globals$`...`)) {
        stop("Did you mean to create the future within a function?  Invalid future expression tries to use global '...' variables that do not exist: ", paste(deparse(exprs), collapse="; "))
      }

      ## BatchJobs do not handle `<future-call-arguments>`, cf.
      ## https://github.com/tudo-r/BatchJobs/issues/93
      globals$`future.call.arguments` <- globals$`...`
      globals$`...` <- NULL

      ## To please R CMD check
      a <- `future.call.arguments` <- NULL
      rm(list=c("a", "future.call.arguments"))

      for (kk in seq_along(exprs)) {
        expr <- exprs[[kk]]
        expr <- substitute({
          do.call(function(...) a, args=`future.call.arguments`)
        }, list(a=expr))
        exprs[[kk]] <- expr
      }
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

    ## Drop all globals which are located in one of
    ## the packages in 'pkgs'.  They will be available
    ## since those packages are attached.
    pkgs <- packagesOf(globals)
    where <- attr(globals, "where")

    names <- names(globals)
    keep <- rep(TRUE, times=length(globals))
    names(keep) <- names
    for (name in names) {
      pkg <- environmentName(where[[name]])
      if (pkg %in% pkgs) keep[name] <- FALSE
    }
    if (!all(keep)) globals <- globals[keep]

    ## ROBUSTNESS: Drop globals that already live in one of
    ## the loaded packages/namespaces.  Should have been
    ## taken care of above, but in case there are any mistakes
    ## we take of it below
    keep <- rep(TRUE, times=length(globals))
    names <- names(globals)
    for (kk in seq_along(globals)) {
      name <- names[kk]
      obj <- globals[[kk]]
      mode <- mode(obj)
      envir <- environment(obj)

      ## Not part of a loaded package / namespace?
      if (is.null(envir)) next
      if (!environmentName(envir) %in% loadedNamespaces()) next

      ## Before deciding to drop, make sure the object with the
      ## the same name truly exist in the environment that it
      ## claims to according to environment().  This will prevent
      ## copies such as FUN <- base::sample from being dropped.
      if (!exists(name, mode=mode, envir=envir, inherits=FALSE)) next
      pkgObj <- get(name, mode=mode, envir=envir, inherits=FALSE)
      if (!identical(pkgObj, obj)) next

      ## Drop - should really not happen with globals (> 0.4.1)
      keep[kk] <- FALSE
    }

    if (!all(keep)) globals <- globals[keep]
    keep <- NULL ## Not needed anymore

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
  ## Any globals to encode/decore to workaround various
  ## BatchJobs limitations.
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  encodeGlobals <- FALSE
  if (length(globals) > 0L) {
    globalsToEncode <- NULL

    ## BatchJobs::batchExport() validated names of globals using
    ## checkmate::assertList(more.args, names="strict") which doesn't
    ## like names such as "{", although they should be valid indeed.
    ## Details: https://github.com/tudo-r/BatchJobs/issues/93
    keep <- grepl("^[.a-zA-Z]", names(globals))
    if (!all(keep)) {
      names <- names(globals)[!keep]
      globalsToEncode <- c(globalsToEncode, names)
      msg <- sprintf("WORKAROUND: BatchJobs does not support exporting of variables with names that does not match pattern '[a-zA-Z0-9._-]+' (see https://github.com/tudo-r/BatchJobs/issues/93). Encoding/decoding the following global variables: %s", hpaste(sQuote(names)))
      if (debug) mcat(msg)
    }

    ## FIXME: The below can be removed with
    ##        fail (>= 1.3) and BatchJobs (>= 1.7)
    ##        /HB 2015-10-20
    ## BatchJobs::loadExports() ignores exported variables that
    ## start with a period.
    ## Details: https://github.com/tudo-r/BatchJobs/issues/103
    bad <- grepl("^[.]", names(globals))
    if (any(bad)) {
      names <- names(globals)[bad]
      globalsToEncode <- c(globalsToEncode, names)
      msg <- sprintf("WORKAROUND: BatchJobs does not support exported variables that start with a period (see https://github.com/tudo-r/BatchJobs/issues/103). Encoding/decoding the following global variables: %s", hpaste(sQuote(names)))
      if (debug) mcat(msg)
    }

    ## Does any globals need to be encoded/decoded to workaround
    ## the limitations of BatchJobs?
    encodeGlobals <- (length(globalsToEncode) > 0L)
    if (encodeGlobals) {
      ## (a) URL encode global variable names
      globalsToDecode <- sapply(globalsToEncode, FUN=utils::URLencode, reserved=TRUE)
      ## (b) Substitute '%' with '_.PERCENT._'
      globalsToDecode <- gsub("%", "_.PERCENT._", globalsToDecode, fixed=TRUE)

      ## (c) Append with 'R_ASYNC_RENAME_'
      globalsToDecode <- paste("R_ASYNC_RENAME_", globalsToDecode, sep="")

      ## (d) Rename corresponding globals
      names <- names(globals)
      idxs <- match(globalsToEncode, names)
      names[idxs] <- globalsToDecode
      names(globals) <- names

      ## (d) Record variables which to be rename by the future
      globals <- append(globals, list(R_ASYNC_GLOBALS_TO_RENAME=globalsToDecode))
    }
  }


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Any globals to export?
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  if (length(globals) > 0L) batchExport(reg, li=globals)
  rm(list=c("globals")) # Not needed anymore


  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Batch process expressions
  ## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  ## Does some globals need to be encoded?
  if (encodeGlobals) {
    fun <- function(expr, ..., envir=globalenv()) {
      eval(substitute({
        ## Decode exported globals (workaround for BatchJobs)
        for (..key.. in R_ASYNC_GLOBALS_TO_RENAME) {
          ..key2.. <- sub("^R_ASYNC_RENAME_", "", ..key..)
          ..key2.. <- gsub("_.PERCENT._", "%", ..key2.., fixed=TRUE)
          ..key2.. <- utils::URLdecode(..key2..)
          assign(..key2.., get(..key.., inherits=FALSE), inherits=FALSE)
        }
        rm(list=c("..key..", "..key2..", "R_ASYNC_GLOBALS_TO_RENAME"))

        local(expr)
      }), envir=envir)
    }
  } else {
    fun <- function(expr, ..., envir=globalenv()) {
      eval(substitute(local(expr)), envir=envir)
    }
  }

  batchMap(reg, fun=fun, exprs, ...)
} # asyncBatchEvalQ()
