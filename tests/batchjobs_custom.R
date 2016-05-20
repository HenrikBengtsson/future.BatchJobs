source("incl/start.R")
library("listenv")

message("*** batchjobs_custom() ...")

## BatchJobs configuration R scripts to be tested
path <- system.file("conf", package="future.BatchJobs")
filenames <- dir(path=path, pattern="[.]R$")

## Don't test SSH backends, because it requires working ssh
## clients and daemons, which might not be the case everywhere
filenames <- setdiff(filenames, "ssh.R")

pathnames <- file.path(path, filenames)


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
