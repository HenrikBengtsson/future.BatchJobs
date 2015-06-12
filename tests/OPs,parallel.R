library("async")
library("R.utils")

## Make sure not to clash with R.utils
`%<-%` <- async::`%<-%`

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
plan(async)
obe <- backend(c("multicore=2", "local"))

rm(list=intersect(c("x", "y"), ls()))

message("** Delayed non-asynchronous evaluation")
mprint(ls())
v0 %<-% { print("Starting"); y <- 1; print("Finished"); y }
mprint(ls())
mprintf("v0=%s\n", v0)
stopifnot(v0 == 1)
mprint(ls())
print(exists("y"))
stopifnot(!exists("y") || !identical(y, 1))

{ print("Starting"); y <- 1; print("Finished"); y } %->% v0
mprint(ls())
mprintf("v0=%s\n", v0)
stopifnot(v0 == 1)
mprint(ls())
print(exists("y"))
stopifnot(!exists("y") || !identical(y, 1))


message("** Delayed asynchronous evaluation without globals")
v1 %<=% { x <- 1 }
stopifnot(!exists("x") || !identical(x, 1))

message("** Delayed asynchronous evaluation with globals")
a <- 2
v2 %<=% { x <- a }
stopifnot(!exists("x") || !identical(x, a))

message("** Delayed asynchronous evaluation with errors")
## Test that async() works when there are errors
v3 %<=% { x <- 3; stop("Woops!"); x }
stopifnot(!exists("x") || !identical(x, 3))

message("** Delayed asynchronous evaluation with progress bar (~5s)")
v4 %<=% {
  ## FIXME: On our cluster outputting to stdout/stderr causes it
  ## to stall. Don't understand why? /HB 2015-02-10
  mprintf <- function(...) {}

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
#stopifnot(v4 == 4)


message("** Left-to-right assignments")
a %<-% 1
mprintf("a=%s\n", a)
1 %->% b
mprintf("b=%s\n", b)
stopifnot(b == a)

c %<=% 1
mprintf("c=%s\n", c)
1 %=>% d
mprintf("d=%s\n", d)
stopifnot(d == c)



message("** Nested asynchronous evaluation")
a %<=% {
  b %<-% 1
  c %<=% 2
  3 %->% d
  4 %=>% e
  b + c + d + e
}
mprintf("a=%s\n", a)
stopifnot(a == 10)

{ a + 1 } %=>% b
mprintf("b=%s\n", b)
stopifnot(b == a + 1)


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())

