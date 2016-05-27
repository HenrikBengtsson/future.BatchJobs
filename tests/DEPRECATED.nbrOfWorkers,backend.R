source("incl/start.R")
library("listenv")

message("*** nbrOfWorkers() ...")

message("*** nbrOfWorkers() - uniprocess ...")

backends <- c("interactive", "local")
for (backend in backends) {
  message(sprintf("- backend('%s')", backend))
  backend(backend)
  n <- nbrOfWorkers()
  message("Number of workers: ", n)
  stopifnot(n == 1L)
}

message("*** nbrOfWorkers() - uniprocess ... DONE")


ncores <- availableCores("multicore")
if (ncores >= 2L) {
message("*** nbrOfWorkers() - multicore ...")

message("- backend('multicore')")
backend("multicore")
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == ncores)

message("- backend('multicore=2')")
backend("multicore=2")
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 2L)

message("*** nbrOfWorkers() - multicore ... DONE")
} ## if (ncores >= 2L)

message("*** nbrOfWorkers() ... DONE")

source("incl/end.R")

