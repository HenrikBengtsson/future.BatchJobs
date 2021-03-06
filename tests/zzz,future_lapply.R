source("incl/start.R")
library("listenv")

if (requireNamespace("future.apply", quietly = TRUE)) {
  future_lapply <- future.apply::future_lapply
  
  strategies <- c("batchjobs_local")

  ## CRAN processing times: Don't run these tests on Windows 32-bit
  if (!fullTest && isWin32) strategies <- character(0L)

  message("*** future_lapply() ...")
  
  message("- future_lapply(x, FUN=vector, ...) ...")
  
  x <- list(a="integer", c="character", c="list")
  str(list(x=x))
  
  y0 <- lapply(x, FUN=vector, length=2L)
  str(list(y0=y0))
  
  for (strategy in strategies) {
    message(sprintf("- plan('%s') ...", strategy))
    plan(strategy)
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN=vector, length=2L, future.scheduling = scheduling)
      str(list(y=y))
      stopifnot(identical(y, y0))
    }
  }
  
  
  message("- future_lapply(x, FUN=base::vector, ...) ...")
  
  x <- list(a="integer", c="character", c="list")
  str(list(x=x))
  
  y0 <- lapply(x, FUN=base::vector, length=2L)
  str(list(y0=y0))
  
  for (strategy in strategies) {
    message(sprintf("- plan('%s') ...", strategy))
    plan(strategy)
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN=base::vector, length=2L, future.scheduling = scheduling)
      str(list(y=y))
      stopifnot(identical(y, y0))
    }
  }
  
  message("- future_lapply(x, FUN=future:::hpaste, ...) ...")
  
  x <- list(a=c("hello", b=1:100))
  str(list(x=x))
  
  y0 <- lapply(x, FUN=future:::hpaste, collapse="; ", maxHead=3L)
  str(list(y0=y0))
  
  for (strategy in strategies) {
    message(sprintf("- plan('%s') ...", strategy))
    plan(strategy)
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN=future:::hpaste, collapse="; ", maxHead=3L, future.scheduling = scheduling)
      str(list(y=y))
      stopifnot(identical(y, y0))
    }
  }
  
  
  message("- future_lapply(x, FUN=listenv::listenv, ...) ...")
  
  x <- list()
  
  y <- listenv()
  y$B <- c("hello", b=1:100)
  x$b <- y
  
  print(x)
  
  y0 <- lapply(x, FUN=listenv::map)
  str(list(y0=y0))
  
  for (strategy in strategies) {
    message(sprintf("- plan('%s') ...", strategy))
    plan(strategy)
    for (scheduling in list(FALSE, TRUE)) {
      y <- future_lapply(x, FUN=listenv::map, future.scheduling = scheduling)
      str(list(y=y))
      stopifnot(identical(y, y0))
    }
  }
  
  
  message("- future_lapply(x, FUN, ...) for large length(x) ...")
  a <- 3.14
  x <- 1:1e6
  
  y <- future_lapply(x, FUN = function(z) sqrt(z + a))
  y <- unlist(y, use.names = FALSE)
  
  stopifnot(all.equal(y, sqrt(x + a)))
  
  
  message("- future_lapply() with global in non-attached package ...")
  library("tools")
  my_ext <- function(x) file_ext(x)
  y_truth <- lapply("abc.txt", FUN = my_ext)
  
  for (strategy in strategies) {
    plan(strategy)
    y <- future_lapply("abc.txt", FUN = my_ext)
    stopifnot(identical(y, y_truth))
  }
  
  message("*** future_lapply() ... DONE")
}

source("incl/end.R")
