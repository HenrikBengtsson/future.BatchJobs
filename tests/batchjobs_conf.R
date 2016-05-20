source("incl/start.R")
library("listenv")

message("*** batchjobs_conf() ...")


## Technically this could give an error if there is a
## malfunctioning ~/.BatchJobs.R on the test system.
message("*** batchjobs_conf() w/out pathnames (default) ...")
plan(batchjobs_conf)

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

message("*** batchjobs_conf() w/out pathnames (default) ... DONE")


message("*** batchjobs_conf() w/ pathnames ...")

## BatchJobs configuration R scripts to be tested
path <- system.file("conf", package="future.BatchJobs")
filenames <- c("local.R", "interactive.R")
if (availableCores("multicore") > 1L) filenames <- c(filenames, "multicore.R")
pathnames <- file.path(path, filenames)

for (pathname in pathnames) {
  message(sprintf("- plan(batchjobs_conf, pathnames='%s') ...", pathname))
  plan(batchjobs_conf, pathnames=pathname)

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

  message(sprintf("- plan(batchjobs_conf, pathnames='%s') ... DONE", pathname))
} ## for (pathname ...)

message("*** batchjobs_conf() w/ pathnames ... DONE")

message("*** batchjobs_conf() ... DONE")

source("incl/end.R")
