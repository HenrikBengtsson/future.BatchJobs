source("incl/start.R")

message("*** plan() ...")

message("*** Set strategy via future::plan(future::batchjobs)")
oplan <- future::plan(future.BatchJobs::batchjobs)
print(future::plan())
future::plan(oplan)
print(future::plan())


library("future.BatchJobs")
backend("local")

for (backend in c("interactive", "local")) {
  message(sprintf("*** plan(batchjobs, backend='%s') ...", backend))

  plan(batchjobs, backend=backend)
  if (exists("tweak", envir=asNamespace("future"))) {
    stopifnot(inherits(plan(), "batchjobs"))
  }

  a <- 0
  f <- future({
    b <- 3
    c <- 2
    a * b * c
  })
  a <- 7  ## Make sure globals are frozen
  v <- value(f)
  print(v)
  stopifnot(v == 0)

  message(sprintf("*** plan(batchjobs, backend='%s') ... DONE", backend))
} # for (backend ...)


message("*** Assert that default backend can be overridden ...")

plan(batchjobs)
backend("local")

## Process ID of main R session
mpid <- Sys.getpid()
print(mpid)

## Process ID of background R session
pid %<-% { Sys.getpid() }
print(pid)
stopifnot(pid != mpid)

## Process ID of background R session
pid %<-% { Sys.getpid() }
print(pid)
stopifnot(pid != mpid)

## Use interactive Batchjobs futures
plan(batchjobs, backend="interactive")
pid %<-% { Sys.getpid() }
print(pid)
stopifnot(pid == mpid)

message("*** Assert that default backend can be overridden ... DONE")


message("*** plan() ... DONE")

source("incl/end.R")
