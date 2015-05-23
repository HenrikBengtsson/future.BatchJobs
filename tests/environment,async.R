library("async")
backend("interactive")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Async delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
z <- new.env()
stopifnot(length(names(z)) == 0L)

message("*** %<=% on environment: Assign by index (not allowed)")
res <- try(z[[1]] %<=% { 2 }, silent=TRUE)
stopifnot(inherits(res, "try-error"))

message("*** %<=% on environment: Assign by name (new)")
z$B %<=% TRUE
stopifnot(length(z) == 2) # sic!
stopifnot(identical(names(z)[1], "B"))

y <- as.list(z)
str(y)
stopifnot(length(y) == 1)
stopifnot(identical(names(y), "B"))


message("*** %<=% on environment: Potential task name clashes")
u <- new.env()
u$a %<=% 1
stopifnot(length(u) == 2)
stopifnot(identical(names(u)[1], "a"))
tu <- inspect(u$a)

v <- new.env()
v$a %<=% 2
stopifnot(length(v) == 2)
stopifnot(identical(names(v)[1], "a"))
tv <- inspect(v$a)
stopifnot(!identical(tu, tv))

tu <- inspect(u$a)
stopifnot(!identical(tu, tv))

stopifnot(identical(u$a, 1))
stopifnot(identical(v$a, 2))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
