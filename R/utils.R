isNA <- function(x) {
  if (length(x) != 1L) return(FALSE)
  is.na(x)
}


stop_if_not <- function(...) {
  res <- list(...)
  for (ii in 1L:length(res)) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "....")
        stop(sprintf("%s is not TRUE", sQuote(call)),
             call. = FALSE, domain = NA)
    }
  }
  
  NULL
}
stop_if_not <- function(...) {
  res <- list(...)
  for (ii in 1L:length(res)) {
    res_ii <- .subset2(res, ii)
    if (length(res_ii) != 1L || is.na(res_ii) || !res_ii) {
        mc <- match.call()
        call <- deparse(mc[[ii + 1]], width.cutoff = 60L)
        if (length(call) > 1L) call <- paste(call[1L], "....")
        stop(sprintf("%s is not TRUE", sQuote(call)),
             call. = FALSE, domain = NA)
    }
  }
  
  NULL
}

attachedPackages <- function() {
  pkgs <- search()
  pkgs <- grep("^package:", pkgs, value=TRUE)
  pkgs <- gsub("^package:", "", pkgs)
  pkgs
}

printf <- function(...) cat(sprintf(...))

mcat <- function(...) message(..., appendLF=FALSE)

mprintf <- function(...) message(sprintf(...), appendLF=FALSE)

mprint <- function(...) {
  bfr <- captureOutput(print(...))
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=FALSE)
}

#' @importFrom utils str
mstr <- function(...) {
  bfr <- captureOutput(str(...))
  bfr <- paste(c(bfr, ""), collapse="\n")
  message(bfr, appendLF=FALSE)
}

## From R.utils 2.0.2 (2015-05-23)
hpaste <- function(..., sep="", collapse=", ", lastCollapse=NULL, maxHead=if (missing(lastCollapse)) 3 else Inf, maxTail=if (is.finite(maxHead)) 1 else Inf, abbreviate="...") {
  if (is.null(lastCollapse)) lastCollapse <- collapse

  # Build vector 'x'
  x <- paste(..., sep=sep)
  n <- length(x)

  # Nothing todo?
  if (n == 0) return(x)
  if (is.null(collapse)) return(x)

  # Abbreviate?
  if (n > maxHead + maxTail + 1) {
    head <- x[seq_len(maxHead)]
    tail <- rev(rev(x)[seq_len(maxTail)])
    x <- c(head, abbreviate, tail)
    n <- length(x)
  }

  if (!is.null(collapse) && n > 1) {
    if (lastCollapse == collapse) {
      x <- paste(x, collapse=collapse)
    } else {
      xT <- paste(x[1:(n-1)], collapse=collapse)
      x <- paste(xT, x[n], sep=lastCollapse)
    }
  }

  x
} # hpaste()

## Adopted R.utils 2.1.0 (2015-06-15)
#' @importFrom utils capture.output
captureOutput <- function(expr, envir=parent.frame(), ...) {
  res <- eval({
    file <- rawConnection(raw(0L), open="w")
    on.exit(close(file))
    capture.output(expr, file=file)
    rawToChar(rawConnectionValue(file))
  }, envir=envir, enclos = baseenv())
  unlist(strsplit(res, split="\n", fixed=TRUE), use.names=FALSE)
}

## Adopted from R.oo 1.19.0 (2015-06-15)
trim <- function(x, ...) {
  sub("[\t\n\f\r ]*$", "", sub("^[\t\n\f\r ]*", "", x))
}


importFuture <- function(name, default=NULL) {
  ns <- getNamespace("future")
  if (exists(name, mode="function", envir=ns, inherits=FALSE)) {
    get(name, mode="function", envir=ns, inherits=FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such 'future' function: %s()", name))
  }
}


importBatchJobs <- function(name, default=NULL) {
  ns <- getNamespace("BatchJobs")
  if (exists(name, mode="function", envir=ns, inherits=FALSE)) {
    get(name, mode="function", envir=ns, inherits=FALSE)
  } else if (!is.null(default)) {
    default
  } else {
    stop(sprintf("No such 'BatchJobs' function: %s()", name))
  }
}

## Evaluates an expression in global environment.
## Because geval() is exported, we want to keep its environment()
## as small as possible, which is why we use local().  Without,
## the environment would be that of the package itself and all of
## the package would be exported.
geval <- local(function(expr, substitute=FALSE, envir=.GlobalEnv, enclos = baseenv(), ...) {
  if (substitute) expr <- substitute(expr)
  eval(expr, envir=envir, enclos = enclos)
})


## Tests if the current OS is of a certain type
isOS <- function(name) {
  if (name == "windows") {
    return(.Platform$OS.type == "windows")
  } else {
    grepl(paste0("^", name), R.version$os)
  }
} ## isOS()


## Suppress:
##   "Warning in result_fetch(res@ptr, n = n) :
##    Don't need to call dbFetch() for statements, only for queries"
## from DBI::dbFetch() when called by BatchJobs::makeRegistry().
## This is because BatchJobs has not been updated according to the
## updates in DBI.
suppressDBIWarnings <- function(expr) {
  ops <- options(warn = -1L)
  on.exit(options(ops))
  withCallingHandlers(expr,
                      warning = function(w) invokeRestart("muffleWarning"))
}


result_has_errors <- function(result) {
  stop_if_not(inherits(result, "FutureResult"))

  ## BACKWARD COMPATIBILITY: future (< 1.11.0)
  if (inherits(result$condition, "error")) return(TRUE)
  
  for (c in result$conditions) {
    if (inherits(c$condition, "error")) return(TRUE)
  }
  
  FALSE
}
