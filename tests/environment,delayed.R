library("async")
## Make sure not to clash with R.utils
`%<-%` <- async::`%<-%`

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=TRUE)
plan(async)
obe <- backend(c("multicore=2", "local"))

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Assign by name
z <- new.env()
z$a %<-% { 1 }
print(names(z))
stopifnot(length(z) == 1L)
stopifnot(identical(names(z), "a"))

z$c %<-% { 3 }
print(names(z))
stopifnot(length(z) == 2L)
stopifnot(identical(names(z), c("a", "c")))

## Cannot set names
res <- try(names(z) <- c("A", "C"), silent=TRUE)
stopifnot(inherits(res, "try-error"))

## Extract as list
y <- as.list(z)
str(y)
stopifnot(length(y) == 2)
stopifnot(identical(names(y), c("a", "c")))

## Assign by new name
z$d %<-% TRUE
print(names(z))
stopifnot(identical(names(z), c("a", "c", "d")))

## Assign by existing name
z$b %<-% TRUE
print(names(z))
stopifnot(identical(names(z), c("a", "b", "c", "d")))

## Potential task name clashes
u <- new.env()
v <- new.env()

u$a %<-% 1
v$a %<-% 2

stopifnot(identical(u$a, 1))
stopifnot(identical(v$a, 2))


## Cannot assign by index
z <- new.env()
res <- try(z[[1]] %<-% { 1 }, silent=TRUE)
stopifnot(inherits(res, "try-error"))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
