library("async")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)


x <- listenv()
x$a %<=% { 1 }

t1 <- inspect("a", envir=x)
t2 <- inspect(a, envir=x)
t3 <- inspect(x[["a"]])
t4 <- inspect(x$a)
t5 <- inspect(x[[1]])
stopifnot(identical(t2, t1), identical(t3, t2),
          identical(t4, t3), identical(t5, t4))


## Out-of-bound subscript, cf. lists
res <- try(inspect(x[[0]]), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Out-of-bound subscript, cf lists
res <- try(inspect(x[[10]]), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Invalid subscript
res <- try(inspect(x[[1+2i]]), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Non-existing object
res <- try(inspect(z[[1]]), silent=TRUE)
stopifnot(inherits(res, "try-error"))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
