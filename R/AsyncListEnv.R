#' Create an async list environment
#'
#' @param ... Passed to \code{listenv()}.
#'
#' @return An environment of class `AsyncListEnv` extending `listenv`.
#'
#' @export
#' @importFrom listenv listenv
AsyncListEnv <- function(...) {
  x <- listenv(...)
  class(x) <- c("AsyncListEnv", class(x))
  x
}

#' Print an AsyncListEnv
#'
#' @param x An AsyncListEnv object
#' @param ... Not used.
#'
#' @export
#' @importFrom R.utils hpaste
#' @keywords internal
print.AsyncListEnv <- function(x, ...) {
  printf("%s:\n", class(x)[1])
  printf("Number of tasks: %d\n", length(x))

  idxs <- which(finished(x))
  if (length(idxs) == 0L) {
    printf("Finished tasks: [0] <none>\n")
  } else {
    labels <- names(idxs)
    if (is.null(labels)) labels <- idxs
    printf("Finished tasks: [%d] %s\n", length(idxs), hpaste(sQuote(labels)))
  }

  idxs <- which(!sapply(error(x), FUN=is.null))
  if (length(idxs) == 0L) {
    printf("Errored tasks: [0] <none>\n")
  } else {
    labels <- names(idxs)
    if (is.null(labels)) labels <- idxs
    printf("Errored tasks: [%d] %s\n", length(idxs), hpaste(sQuote(labels)))
  }
}

#' Inspects all elements of AsyncListEnv
#'
#' @param x An AsyncListEnv object
#' @param ... Arguments passed to \code{inspect()}.
#'
#' @return A list of inspect values.
#'
#' @export
#' @keywords internal
inspectAll <- function(...) UseMethod("inspectAll")

#' @export
inspectAll.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- vector("list", length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    res[[kk]] <- inspect(x[[kk]], ...)
  }
  res
}

#' @export
status.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- vector("list", length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- inspect(x[[kk]], ...)
    if (inherits(task, "AsyncTask"))
      res[[kk]] <- status(task)
  }
  res
}

#' @export
finished.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- logical(length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- inspect(x[[kk]], ...)
    if (inherits(task, "AsyncTask"))
      res[kk] <- finished(task)
  }
  res
}

#' @export
error.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- vector("list", length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- inspect(x[[kk]], ...)
    if (inherits(task, "AsyncTask"))
      res[kk] <- list(error(task))
  }
  res
}

#' @export
value.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- vector("list", length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- inspect(x[[kk]], ...)
    if (inherits(task, "AsyncTask"))
      res[kk] <- list(value(task))
  }
  res
}

