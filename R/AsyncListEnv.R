#' Create an async list environment
#'
#' @param \ldots Passed to \code{listenv()}.
#'
#' @return An environment of class `AsyncListEnv` extending `listenv`.
#'
#' @export
#' @importFrom listenv listenv
#' @importFrom future futureOf
AsyncListEnv <- function(...) {
  x <- listenv(...)
  class(x) <- c("AsyncListEnv", class(x))
  x
}

#' Print an AsyncListEnv
#'
#' @param x An AsyncListEnv object
#' @param \ldots Not used.
#'
#' @export
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

  idxs <- which(failed(x))
  if (length(idxs) == 0L) {
    printf("Failed tasks: [0] <none>\n")
  } else {
    labels <- names(idxs)
    if (is.null(labels)) labels <- idxs
    printf("Failed tasks: [%d] %s\n", length(idxs), hpaste(sQuote(labels)))
  }

  idxs <- which(expired(x))
  if (length(idxs) == 0L) {
    printf("Expired tasks: [0] <none>\n")
  } else {
    ## covr: skip=3
    labels <- names(idxs)
    if (is.null(labels)) labels <- idxs
    printf("Expired tasks: [%d] %s\n", length(idxs), hpaste(sQuote(labels)))
  }
}


#' @export
status.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- vector("list", length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- futureOf(x[[kk]], ..., mustExist=FALSE)
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
    task <- futureOf(x[[kk]], ..., mustExist=FALSE)
    if (inherits(task, "AsyncTask"))
      res[kk] <- finished(task)
  }
  res
}

#' @export
completed.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- logical(length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- futureOf(x[[kk]], ..., mustExist=FALSE)
    if (inherits(task, "AsyncTask"))
      res[kk] <- completed(task)
  }
  res
}

#' @export
failed.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- logical(length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- futureOf(x[[kk]], ..., mustExist=FALSE)
    if (inherits(task, "AsyncTask"))
      res[kk] <- failed(task)
  }
  res
}

#' @export
expired.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- logical(length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- futureOf(x[[kk]], ..., mustExist=FALSE)
    if (inherits(task, "AsyncTask"))
      res[kk] <- expired(task)
  }
  res
}

#' @export
error.AsyncListEnv <- function(x, ...) {
  nx <- length(x)
  res <- vector("list", length=nx)
  names(res) <- names(x)
  for (kk in seq_len(nx)) {
    task <- futureOf(x[[kk]], ..., mustExist=FALSE)
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
    task <- futureOf(x[[kk]], ..., mustExist=FALSE)
    if (inherits(task, "AsyncTask"))
      res[kk] <- list(value(task))
  }
  res
}

