R.utils::use()
use("async")

oopts <- options(warn=1, "async::debug"=FALSE)

message("** Delayed non-asynchronous evaluation")
x %<-% { print("Starting"); y <- 1; print("Finished"); y }
mprintf("x=%s\n", x)
stopifnot(x == 1)


message("** Delayed asynchronous evaluation without globals")
v1 %<=% { x <- 1 }

message("** Delayed asynchronous evaluation with globals")
a <- 2
v2 %<=% { x <- a }

message("** Delayed asynchronous evaluation with errors")
## Test that async() works when there are errors
v3 %<=% ({ x <- 3; stop("Woops!"); x })

message("** Delayed asynchronous evaluation with progress bar (~5s)")
v4 %<=% {
  library("R.utils")
  mprintf("Processing: ")
  for (ii in 1:10) { mprintf("."); Sys.sleep(0.5) }
  mprintf(" [100%%]\n")
  4
}

message("** Collecting results")
mprintf("v1=%s\n", v1)
stopifnot(v1 == 1)

mprintf("v2=%s\n", v2)
stopifnot(v2 == a)

stopifnot(tryCatch({
  mprintf("v3=%s\n", v3)
}, error = function(ex) {
  mcat("v3: Error as expect!\n")
  TRUE
}))

mprintf("v4=%s\n", v4)
stopifnot(v4 == 4)


## Cleanup
options(oopts)
