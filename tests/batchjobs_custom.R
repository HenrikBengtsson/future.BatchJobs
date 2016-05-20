source("incl/start.R")
library("listenv")

message("*** batchjobs_custom() ...")

path <- system.file("conf", package="future.BatchJobs")
pathnames <- dir(path=path, pattern="[.]R$", full.names=TRUE)

for (pathname in pathnames) {
  message(sprintf("- plan(batchjobs_custom, pathname='%s') ...", pathname))
  plan(batchjobs_custom, pathname=pathname)

  f <- future({
    42L
  })
  stopifnot(inherits(f, "BatchJobsFuture"))
  y <- value(f)
  print(y)
  stopifnot(y == 42L)

  ## A global variable
  a <- 0
  f <- future({
    b <- 3
    c <- 2
    a * b * c
  })
  print(f)

  a <- 7  ## Make sure globals are frozen
  v <- value(f)
  print(v)
  stopifnot(v == 0)

  message(sprintf("- plan(batchjobs_custom, pathname='%s') ... DONE", pathname))
} ## for (pathname ...)


message("*** batchjobs_custom() ... DONE")

source("incl/end.R")
