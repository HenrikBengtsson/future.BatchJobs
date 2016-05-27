source("incl/start.R")
library("listenv")

message("*** nbrOfWorkers() ...")

message("*** nbrOfWorkers() - local, interactive ...")

n <- nbrOfWorkers(batchjobs_local)
message("Number of workers: ", n)
stopifnot(n == 1L)

n <- nbrOfWorkers(batchjobs_interactive)
message("Number of workers: ", n)
stopifnot(n == 1L)

plan(batchjobs_local)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

plan(batchjobs_interactive)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

message("*** nbrOfWorkers() - local, interactive ... DONE")


ncores <- availableCores("multicore")
if (ncores >= 2L) {
message("*** nbrOfWorkers() - multicore ...")

n <- nbrOfWorkers(batchjobs_multicore)
message("Number of workers: ", n)
stopifnot(n == 1L)

plan(batchjobs_multicore)
n <- nbrOfWorkers()
message("Number of workers: ", n)
stopifnot(n == 1L)

message("*** nbrOfWorkers() - multicore ... DONE")
} ## if (ncores >= 2L)

message("*** nbrOfWorkers() ... DONE")

source("incl/end.R")
