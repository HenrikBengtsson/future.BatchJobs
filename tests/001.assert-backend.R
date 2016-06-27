source("incl/start.R")
library("listenv")

message("*** Assert backends ...")

mcat("Backends to be tested:\n")
if (FALSE && fullTest) {
  TEST_BACKENDS <- c("interactive", "local", "multicore=2")
  future.BatchJobs::backend(c("multicore=2", "local"))
} else {
  TEST_BACKENDS <- c("interactive", "local")
  future.BatchJobs::backend("local")
}
mprint(TEST_BACKENDS)

env <- new.env()
lenv <- listenv()
for (be in TEST_BACKENDS) {
  message("*** Backend: ", be)
  plan(batchjobs, backend=be)

  message(" - Future and promise")
  f <- future(be)
  print(f)
  v <- value(f)
  print(v)
  stopifnot(v == be)

  message(" - Future evaluation (current environment)")
  a %<-% be
  print(a)
  stopifnot(a == be)

  message(" - Future evaluation (environment)")
  env$b %<-% be
  print(env$b)
  stopifnot(env$b == be)

  message(" - Future evaluation (list environment - name)")
  lenv$b %<-% be
  print(lenv$b)
  stopifnot(lenv$b == be)

  message(" - Future evaluation (list environment - index)")
  lenv[[3]] %<-% be
  print(lenv[[3]])
  stopifnot(lenv[[3]] == be)
}

message("*** Assert backends ... DONE")

source("incl/end.R")
