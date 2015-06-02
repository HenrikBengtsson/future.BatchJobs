library("async")

ovars <- ls(envir=globalenv())
oopts <- options(future=async, warn=1, "async::debug"=TRUE)
obe <- backend(c("multicore=2", "local"))


x <- listenv()
x$a %<=% { 1 }

t1 <- inspect("a", envir=x)
t2 <- inspect(a, envir=x)
t3 <- inspect(1, envir=x)
t4 <- inspect(x[["a"]])
t5 <- inspect(x$a)
t6 <- inspect(x[[1]])
stopifnot(identical(t2, t1), identical(t3, t2), identical(t4, t3),
          identical(t5, t4), identical(t6, t5))

tasks <- inspect(envir=x)

## Out-of-bound subscript, cf. lists
stopifnot(is.na(inspect(x[[0]])))
res <- try(inspect(x[[0]], mustExist=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Out-of-bound subscript, cf lists
stopifnot(is.na(inspect(x[[10]])))
res <- try(inspect(x[[10]], mustExist=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Invalid subscript
res <- try(inspect(x[[1+2i]], mustExist=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Non-existing object
res <- try(inspect(z[[1]], mustExist=TRUE), silent=TRUE)
stopifnot(inherits(res, "try-error"))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
