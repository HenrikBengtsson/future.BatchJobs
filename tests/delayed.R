R.utils::use()
use("async")

oopts <- options(warn=1, "async::debug"=FALSE)

message("** Delayed non-asynchronous evaluation")
x %<-% { print("Starting"); a <- 1; print("Finished"); a }

message("** Delayed asynchronous evaluation #1")
y %<-||% {
  library("R.utils")
  mprintf("Processing: ")
  for (ii in 1:10) { mprintf("."); Sys.sleep(1) }
  mprintf(" [100%%]\n")
  TRUE
}

message("** Delayed asynchronous evaluation #2")
z %<-||% { print("Starting"); b <- 2; print("Finished"); b }

message("** Collecting results")
mprintf("x=%s\n", x)
stopifnot(x == 1)

mprintf("y=%s\n", y)
stopifnot(y == TRUE)

mprintf("z=%s\n", z)
stopifnot(z == 2)


## Cleanup
options(oopts)
