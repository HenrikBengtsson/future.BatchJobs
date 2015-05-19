library("async")
backend("local")

ovars <- ls(envir=globalenv())
oopts <- options(warn=1, "async::debug"=FALSE)

## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## Async delayed assignment (infix operator)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - -
z <- listenv()

## Assign by index
z[[1]] %<=% { 2 }
z[[4]] %<=% { "async!" }
stopifnot(length(z) == 4)

## Add names
names(z) <- c("A", "B", "C", "D")
stopifnot(identical(names(z), c("A", "B", "C", "D")))

## Assign by name
z$B %<=% TRUE
stopifnot(length(z) == 4)
stopifnot(identical(names(z), c("A", "B", "C", "D")))

y <- as.list(z)
str(y)
stopifnot(length(y) == 4)
stopifnot(identical(names(y), c("A", "B", "C", "D")))


## Potential task name clashes
u <- listenv()
v <- listenv()

u$a %<=% 1
v$a %<=% 2

tu <- inspect(u$a)
tv <- inspect(v$a)
## FIXME: inspect() finds the wrong AsyncTask (Issue #12)
stopifnot(!identical(tu, tv))

stopifnot(identical(u$a, 1))
stopifnot(identical(v$a, 2))


## Cleanup
options(oopts)
rm(list=setdiff(ls(envir=globalenv()), ovars), envir=globalenv())
