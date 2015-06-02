library("async")
library("R.utils")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
obe <- backend(c("multicore=2", "local"))

## Test that async() works when there are no globals
t1 <- async({ x <- 1 })
print(t1)
v1 <- await(t1)
print(v1)
stopifnot(v1 == 1)


## Test that async() works when there are globals
a <- 2
t2 <- async({ x <- a })
print(t2)
v2 <- await(t2)
print(v2)
stopifnot(v2 == a)


## Test that async() works when there are errors
t3 <- async({ x <- 3; stop("Woops!"); x })
print(t3)
res <- tryCatch({
  v3 <- await(t3)
  print(v3)
  v3
}, error = function(ex) {
  cat(ex$message)
  cat("\nTEST OK!\n")
  NULL
})
stopifnot(is.null(res))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
