library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
obe <- backend(c("multicore=2", "local"))

delayedAssign("a", {
  ## This message will be outputted
  cat("Delayed assignment evaluated\n")
  1
})

delayedAsyncAssign("b", {
  ## This message will *not* be outputted, because it
  ## is evaluated in a separate process elsewhere where
  ## the output ends up
  cat("Delayed asynchroneous assignment evaluated\n")
  2
})

cat(sprintf("b=%s\n", b))

## The evaluation of 'a' is evaluated at this point
cat(sprintf("a=%s\n", a))

stopifnot(identical(a, 1))
stopifnot(identical(b, 2))


## Potential task name clashes
u <- new.env()
v <- new.env()
delayedAsyncAssign("a", { 2 }, assign.env=u)
delayedAsyncAssign("a", { 4 }, assign.env=v)

cat(sprintf("u$a=%s\n", u$a))
cat(sprintf("v$a=%s\n", v$a))

stopifnot(identical(u$a, 2))
stopifnot(identical(v$a, 4))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
